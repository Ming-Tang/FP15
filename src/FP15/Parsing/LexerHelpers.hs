{-# LANGUAGE BangPatterns #-}
module FP15.Parsing.LexerHelpers where
import Control.Monad.Error(throwError)
import Data.Char(isDigit)
import Data.List.Split
import FP15.Types(Name(..))
import FP15.Parsing.Types

type TokenResult = Either String TokenData
-- | A 'TokenAction' is a function that takes a parsed string, and returns
-- a partially-applied Token constructor.
type TokenAction = String -> TokenResult

-- | The 'illegalToken' function emits an @Illegal@ token with the specified
-- diagnostic message.
illegalToken :: String -> Either String TokenData
illegalToken msg = return $ Illegal msg

readString :: String -> TokenResult

-- $doctests
-- >>> let testStr = (\(StringLiteral s) -> s) . (\(Right t) -> t)
-- >>> let getQN' x = case x of { Functional n -> n; Function n -> n; Operator n -> n; _ -> undefined }
-- >>> let getQN = (\(Right x) -> getQN' x)
-- >>> let testQN = (\(N m n) -> (m, n)) . getQN
-- >>> testStr $ readString "\"\""
-- ""
-- >>> testStr $ readString "\"test\""
-- "test"
-- >>> testStr $ readString "\"te\\\"st\""
-- "te\"st"
-- >>> testStr $ readString "\"\\\"\\\\\n\\t\""
-- "\"\\\n\t"
-- >>> testQN $ readOperator "++"
-- ([],"++")
-- >>> testQN $ readOperator "."
-- ([],".")
-- >>> testQN $ readOperator "..."
-- ([],"...")
-- >>> testQN $ readOperator "Abc.Def.(++)"
-- (["Abc","Def"],"++")
-- >>> testQN $ readOperator "Abc.Def.(...)"
-- (["Abc","Def"],"...")
-- >>> testQN $ readFunction "test"
-- ([],"test")
-- >>> testQN $ readFunction "Abc.Def.test"
-- (["Abc","Def"],"test")
-- >>> testQN $ readFunctional "Test"
-- ([],"Test")
-- >>> testQN $ readFunctional "Abc.Def.Test"
-- (["Abc","Def"],"Test")

readOperator, readFunction, readFunctional :: String -> TokenResult
readNumber' :: Bool -> String -> TokenResult
readNumber, readHash :: String -> TokenResult

readString s0@('"':s) = parse s "" where
  parse :: String -> String -> TokenResult
  parseEscape :: String -> String -> TokenResult

  parse [] _ = throwError "Impossible."
  parse "\"" !ss = return $ StringLiteral $ reverse ss
  parse ('\\':cs) !ss = parseEscape cs ss
  parse ('\"':_) _ = throwError "Impossible."
  parse (c:cs) ss = parse cs (c:ss)

  parseEscape [] _ = illegalToken "Incomplete escape."
  parseEscape (c:(!cs)) !ss =
    case lookup c escapeTable of
      Just c' -> parse cs (c':ss)
      Nothing -> illegalToken $ "Invalid escape: \\" ++ show c ++ ""

  escapeTable = zip "0abfnrtv\"\\" "\0\a\b\f\n\r\t\v\"\\"

readString s = illegalToken "Empty string."

-- | The 'readQualified' function splits an identifier into module part (list of
-- strings) and name part (string).
readName :: (Name a -> TokenData) -> String -> TokenResult

readName !tc s =
  case splitOn "." s of
               [] -> throwError "Invalid qualified name."
               xs -> ret (init xs) (last xs)
  where ret ms n = return $ tc $ N ms n

readDotOperator :: Monad m => String -> m TokenData
readDotOperator dots@('.':(!s)) =
  case splitOn "." s of
    [] -> error "readDotOperator: splitOn returned an empty list."
    xs -> return $ DotOperator $ N (init xs) (last xs)

readDotOperator _ = error "readDotOperator: Not a dot operator"

readOperator !s
  | all (== '.') s =
    return $ Operator $ N [] s
  | '(' `elem` s =
    case splitOn "(" s of
      [m, n] ->
        return $ Operator $ N (init $ splitOn "." m) $ init n
      _ -> illegalToken "Invalid qualified operator."
  | otherwise =
    return $ Operator $ N [] s

readFunction = readName Function
readFunctional = readName Functional

readInt, readReal :: Bool -> String -> TokenData
readInt n = IntLiteral . (if n then negate else id) . read
readReal n = RealLiteral . (if n then negate else id) . read

readNumber ('~':s) = readNumber' True s
readNumber s = readNumber' False s
readNumber' n ('#':'o':oct) = return $ readInt n $ "0o" ++ oct
readNumber' n ('#':'O':oct) = return $ readInt n $ "0o" ++ oct
readNumber' n ('#':'x':hex) = return $ readInt n $ "0x" ++ hex
readNumber' n ('#':'X':hex) = return $ readInt n $ "0x" ++ hex

readNumber' n s | 'e' `elem` s || 'E' `elem` s || '.' `elem` s = return $ readReal n s
             | otherwise = return $ readInt n s

readHash "#" = return Hash
readHash "#t" = return TrueLiteral
readHash "#f" = return FalseLiteral
readHash s0@('#':s@(_:_)) | all isDigit s = return $ Indexer (read s)
                          | otherwise = illegalToken "Invalid #form."
readHash s0 = illegalToken "Invalid #form."

-- | The 'getIndentChange' function gets the change in indentation given
-- whitespaces, which might or might not contain line breaks. If no line breaks,
-- then the whitespaces, return @Nothing@. Othwreise, return the indentation of
-- the last line, which is the number of spaces in the last line, of the
-- input string.
--
-- >>> getIndentChange ""
-- Nothing
-- >>> getIndentChange "  "
-- Nothing
-- >>> getIndentChange "\n"
-- Just 0
-- >>> getIndentChange "  \n"
-- Just 0
-- >>> getIndentChange "  \n    "
-- Just 4
-- >>> getIndentChange "  \n \n     \n    "
-- Just 4
getIndentChange :: String -> Maybe Int
getIndentChange !s =
  case splitOn "\n" s of
    [] -> Nothing
    [_] -> Nothing
    ss -> Just $ length $ last ss

handleIndentChange, popIndentStack
  :: StateStack -> Int -> Either String (StateStack, [TokenData])

handleIndentChange [] _ = Left "Empty state stack."

handleIndentChange ss@(StateItem Offside ref:_) ind
  | ref == ind = Right (ss, [Semicolon])
  | ref < ind = Right (ss, [])
  | otherwise = popIndentStack ss ind

handleIndentChange ss _ = Right (ss, [])

popIndentStack ss _ = Right (ss, [])
