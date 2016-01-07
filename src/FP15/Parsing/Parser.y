{
{-# LANGUAGE Trustworthy #-}

module FP15.Parsing.Parser (parse) where
import qualified Data.Map.Strict as M
import Control.Monad.Error
import Data.Map.Strict(Map)
import Data.Maybe
import Data.List.Split
import FP15.Parsing.Types
import FP15.Parsing.Lexer
import FP15.Types hiding (Get, With, WithLeft, WithRight)
import FP15.Value
}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Either String }

%token
eof { Token EOF _ _ }
"{" { Token LBrace _ _ }
"}" { Token RBrace _ _ }
"[" { Token LBracket _ _ }
"]" { Token RBracket _ _ }
"(" { Token LParen _ _ }
")" { Token RParen _ _ }
"|" { Token Pipe _ _ }
":" { Token Colon _ _ }
"," { Token Comma _ _ }
";" { Token Semicolon _ _ }
"#" { Token Hash _ _ }
"#:" { Token Let _ _ }
"#=" { Token With _ _ }
"#<" { Token WithLeft _ _ }
"#>" { Token WithRight _ _ }
"=" { (viewOperatorS "=" -> Just $$) }

get { Token (Get $$) _ _ }

function { (viewFunction -> Just $$) }
functional { (viewFunctional -> Just $$) }
op { (viewOperator -> Just $$) }
dotOperator { (viewDotOperator -> Just $$) }

false { Token FalseLiteral _ _ }
true { Token TrueLiteral _ _ }
char { Token (CharLiteral $$) _ _ }
int { Token (IntLiteral $$) _ _ }
real { Token (RealLiteral $$) _ _ }
symbol { Token (SymbolLiteral $$) _ _ }
string { Token (StringLiteral $$) _ _ }
indexer { Token (Indexer $$) _ _ }

%%

prog : binding_list eof { ModuleAST { astMN = (M ["Main"])
                                    , astFs = (M.fromList $1)
                                    , astFls = M.empty
                                    , astFFixes = M.empty
                                    , astFlFixes = M.empty
                                    , astImps = []
                                    , astExps = []
                                    } }

operator : "=" { $1 }
         | op { $1 }

binding_list : binding { [$1] }
             | binding_list ";" binding { $1 ++ [$3] }

binding : function "=" expr { (locNameToId $1, $3) }

expr : nonif_expr { $1 }
     | nonif_expr ":" nonif_expr "|" expr { TIf $1 $3 $5 }

nonif_expr : primary_or_op_seq { primList $1 }

primary_op : primary { $1 }
           | operator { TOperator $1 }
           | dotOperator { TDotOperator $1 }

primary_or_op_seq : primary_op { [$1] }
                  | primary_or_op_seq primary_op { $1 ++ [$2] }

f_infix_item : primary_op { Just $1 }
             | "," { Nothing }

f_infix_body : f_infix_item { [$1] }
             | f_infix_body f_infix_item { $1 ++ [$2] }

commas : "," { [] }
       | commas "," { TId : $1 }

expr_list_head : expr_list_head commas expr { $1 ++ $2 ++ [$3] }
               | expr { [$1] }

-- TODO leading comma doesn't work
expr_list : expr_list_head { $1 }
          | expr_list_head commas { $1 ++ TId : $2 }
          | commas expr_list_head { TId : $1 ++ $2 }
          | commas expr_list_head commas { TId:$1 ++ $2 ++ TId:$3 }
          | commas { TId:TId:$1 }
          | { [] }

primary : function { TFunc $1 }
        | literal { $1 }
        | indexer { TIndex $1 }
        | get { TGet $1 }
        | "#=" primary { TWith $2 }
        | "#<" primary { TWithLeft $2 }
        | "#>" primary { TWithRight $2 }

        | "#:" "[" binding_list "]" primary { TLet $3 $5 }
        | "(" f_infix_body ")" { parseFInfix $2 }
        | "(" functional primary_or_op_seq ")" { TApp $2 $3 }
        | "[" expr_list "]" { TFork $2 }
        | "{" expr_list "}" { braces $2 }

literal : false  { TValue $ Bool False }
        | true  { TValue $ Bool True }
        | char { TValue $ Char $1 }
        | int  { TValue $ Int $1 }
        | real  { TValue $ Real $1 }
        | symbol  { TValue $ Symbol $1 }
        | string { TValue $ String $1 }

{
-- | The 'parse' function parses an FP15 source code and returns the
-- parsed module body and imports declared.
parse :: ModuleSource -> Either String ModuleAST

parse (ModuleSource f s) = scanTokensWithFile f s >>= parser

parseError :: [Token] -> Either String a
parseError ts = throwError $ "Parse error with tokens " ++ (show $ take 2 ts) ++ "."

primList [x] = x
primList xs = TUnresolvedPrimaryList xs

braces :: [ExprAST] -> ExprAST
braces [x] = x
braces xs = THook xs

viewFunction :: Token -> Maybe (RLocName F)
viewFunctional :: Token -> Maybe (RLocName Fl)
viewOperator :: Token -> Maybe (RLocName Unknown)

viewFunction (Token (Function n) p _) = Just (Loc (Just p) n)
viewFunction _ = Nothing

viewFunctional (Token (Functional n) p _) = Just (Loc (Just p) n)
viewFunctional _ = Nothing

viewOperator (Token (Operator n) p _) = Just (Loc (Just p) n)
viewOperator _ = Nothing

viewDotOperator (Token (DotOperator n) p _) = Just (Loc (Just p) n)
viewDotOperator _ = Nothing

viewOperatorS :: String -> Token -> Maybe (RLocName Unknown)
viewOperatorS n' (Token (Operator n0@(N [] n)) p _) | n == n' = Just (Loc (Just p) n0)
viewOperatorS _ _ = Nothing

parseFInfix :: [Maybe ExprAST] -> ExprAST
parseFInfix xs
  | all isJust xs = TUnresolvedInfixNotation $ map fromJust xs
  | otherwise =
    let merge ys | all isNothing ys = [Left $ length ys]
                 | otherwise = map (Right . fromJust) ys
        merged = map merge $ split (dropBlanks $ whenElt isNothing) xs in
    TUnresolvedCommaNotation $ concat merged


-- TODO error reporting for things like Module.abc = def
locNameToId :: RLocName F -> LocId F
locNameToId (Loc l (N _ n)) = Loc l (Id n)

}
