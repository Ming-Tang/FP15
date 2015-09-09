{
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module FP15.Parsing.Lexer (
  scanTokens, scanTokensWithFile
) where
import Data.List(intercalate)
import Control.Applicative(Applicative(..))
import Control.Monad(liftM, liftM2, mplus)
import FP15.Types(SrcPos(..))
import FP15.Parsing.Types
import FP15.Parsing.LexerHelpers
}

%wrapper "monadUserState"

-- Character classes
$ws = [ \t\v]
$digit = [0-9]
$letter = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]

$ident_char = [_ $digit $letter]

$op_char = [\@\!\?\~\%\^\&\*\-\+\=\<\>\/\\]

$neg = \~

-- Regular Expressions

@underscores = "_"+
@module_name = @underscores? $upper $ident_char*
@module_part = @module_name ("." @module_name)* "."

@function_part = @underscores | @underscores? $lower $ident_char*
@functional_part = @underscores? $upper $ident_char*
@operator_part = $op_char+ | "."+
@operator = (@module_part \( @operator_part \)
            | @operator_part)

@dot_operator = "." @module_part? @function_part

@string = \" ([^\\\"]|\\.)* \"

@dec = 0|[1-9][0-9]*
@oct = ("#o"|"#O") [0-7]+
@hex = ("#x"|"#X") [0-9a-fA-F]+
@integer = $neg? (@hex | @dec | @oct)

@char = \#\\ ($ident_char+ | .)

@frac = "." [0-9]+
@exp = [eE] [\+\-]? [0-9]+
@real = $neg? @dec (@frac @exp | @frac | @exp)

@number = @integer | @real
-- TODO char literals
@hash = \# ($op_char | $ident_char+)

@newlines = ($ws* \n)+ $ws*

tokens :-
  $white+ { whitespaces }
  "--" .*  ;
  @string { act readString }
  @operator { act readOperator }
  @dot_operator { act readDotOperator }
  @module_part? @functional_part { act readFunctional }
  @module_part? @function_part { act readFunction }
  @number { act readNumber }
  @char { act readChar }
  @hash { act readHash }
  \[ { push Bracket LBracket }
  \] { pop Bracket RBracket }
  \{ { push Brace LBrace }
  \} { pop Brace RBrace }
  \( { push Paren LParen }
  \) { pop Paren RParen }
  -- TODO eliminate them in favor of ternary operators
  \| { act $ const $ return Pipe }
  \: { act $ const $ return Colon }
  \, { act $ const $ return Comma }
  \; { act $ const $ return Semicolon }
  \$ { act $ const $ return Dollar }
  \' { act $ const $ return Quote }
  . { act $ const $ return $ Illegal "Unexpected character." }

{

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = liftM2 ($)

type AlexUserState = State

alexInitUserState :: AlexUserState
alexInitUserState = Normal { stateStack = [StateItem Offside 0]
                           , pendingIndent = Nothing
                           , currentFile = Nothing }

getPendingIndent :: Alex (Maybe Int)
setPendingIndent :: Maybe Int -> Alex ()
pushPendingIndent :: Int -> Alex ()
popPendingIndent :: Alex (Maybe Int)

getPendingIndent = do
  Normal { pendingIndent = p } <- alexGetUserState
  return p

setPendingIndent p' = do
  st@(Normal { pendingIndent = p }) <- alexGetUserState
  alexSetUserState st { pendingIndent = p' }

pushPendingIndent = setPendingIndent . Just
popPendingIndent = do
  p <- getPendingIndent
  setPendingIndent Nothing
  return p

getStateStack :: Alex StateStack
setStateStack :: StateStack -> Alex ()

getStateStack = stateStack `liftM` alexGetUserState

setStateStack ss' = do
  st <- alexGetUserState
  alexSetUserState st { stateStack = ss' }

getCurrentFile :: Alex (Maybe String)
getCurrentFile = do
  st <- alexGetUserState
  return $ currentFile st

setCurrentFile :: Maybe String -> Alex ()
setCurrentFile f' = do
  st <- alexGetUserState
  alexSetUserState $ st { currentFile = f' }

commitIndentChange :: Alex [TokenData]
commitIndentChange =
  popPendingIndent >>= maybe (return []) (\p' -> do
    ss <- getStateStack
    case handleIndentChange ss p' of
      Left e -> alexError e
      Right (ss', ts) -> do
        setStateStack ss'
        return ts)

alexEOF :: Alex [Token]
alexEOF = do
  ss <- getStateStack
  case ss of
    [] -> alexError "Empty state stack."
    _ -> let sts = map stateTag ss
             unclosed = filter (/= Offside) sts
             closings = map getClosing unclosed
             getClosing Offside = ""
             getClosing Bracket = "]"
             getClosing Paren = ")"
             getClosing Brace = "}" in
          case unclosed of
            [] -> return $ [Token EOF (SrcPos 0 0 0 Nothing) ""]
            _ -> alexError $ "Unclosed parens: " ++ intercalate " " closings

push, pop :: StateTag -> TokenData
             -> (AlexPosn, Char, [Byte], String) -> Int -> Alex [Token]
push s f input@(!(AlexPn _ _ col, _, _, _)) !n = do
  ss <- getStateStack
  ts <- act (return . const f) input n
  setStateStack (StateItem s col:ss)
  return ts

pop s f !input !n = do
  ss <- getStateStack
  case ss of
    [] -> alexError "Unexpected closing token."
    (StateItem Offside _):_ -> alexError "Unexpected closing token."
    ((StateItem s' _):ss') ->
      if s == s' then do
        ts <- act (return . const f) input n
        setStateStack ss'
        return ts
      else
        alexError "Closing token mismatch."

getTokenInfo :: (AlexPosn, Char, [Byte], String) -> Int -> Alex (SrcPos, String)
getTokenInfo !(AlexPn p l c, _, _, str) !n = do
  let tokStr = take n str
  return (SrcPos p l c Nothing, tokStr)

-- | Create an @AlexAction@ from a function that takes a string and returns a
-- token or error message as a result, and if a token was returned, indentation
-- will be committed before returning the token.
act :: (String -> Either String TokenData)
       -> (AlexPosn, Char, [Byte], String) -> Int -> Alex [Token]
act f !input !n = do
  indentTs <- commitIndentChange
  (pos, tokStr) <- getTokenInfo input n
  let indentTs' = map (\t -> Token t pos "") indentTs
  case f tokStr of
    Left e -> alexError e
    Right t -> return $ indentTs' ++ [Token t pos tokStr]

whitespaces :: (AlexPosn, Char, [Byte], String) -> Int -> Alex [Token]
whitespaces !input !n = do
  (_, tokStr) <- getTokenInfo input n
  let ic = getIndentChange tokStr
  p <- getPendingIndent
  -- ic, which is the NEW indent, overrides the OLD indent, pi
  setPendingIndent $ ic `mplus` p
  skip input n

scanTokensWithFile :: (Maybe String) -> String -> Either String [Token]
scanTokensWithFile f str = runAlex str loop where
  loop = do
    setCurrentFile f
    ts <- alexMonadScan
    if any isEOF ts then
      return ts
    else
      (ts ++) `liftM` loop
  isEOF (Token EOF _ _) = True
  isEOF _ = False

scanTokens :: String -> Either String [Token]
scanTokens str = scanTokensWithFile Nothing str

}
