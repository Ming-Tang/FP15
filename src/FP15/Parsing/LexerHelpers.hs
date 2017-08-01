module FP15.Parsing.LexerHelpers where
import Control.Monad(mplus)
import Control.Monad.Except(throwError)
import Data.Char(isDigit)
import Data.List.Split
import FP15.Types(Name(..), Map)
import FP15.Parsing.Types
import qualified Data.Map as M

type TokenResult = Either String TokenData
-- | A 'TokenAction' is a function that takes a parsed string, and returns
-- a partially-applied Token constructor.
type TokenAction = String -> TokenResult

octDigits, hexDigits :: String
octDigits = ['0'..'7']
hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

charTable :: Map String Char
charTable =
  M.fromList [
    ("alarm", '\x0007')
  , ("backspace", '\x0008')
  , ("delete", '\x007f')
  , ("escape", '\x001b')
  , ("linefeed", '\x000a')
  , ("newline", '\x000a')
  , ("nul", '\x0000')
  , ("null", '\x0000')
  , ("page", '\x000c')
  , ("return", '\x000d')
  , ("rubout", '\x007f')
  , ("space", ' ')
  , ("tab", '\t')
  , ("vtab", '\v')
  ]

{--
#\alarm
#\backspace
#\delete
#\escape
#\newline
#\null
#\return
#\space
#\tab
; U+0007
; U+0008
; U+007F
; U+001B
; the linefeed character, U+000A ; the null character, U+0000
; the return character, U+000D
; the preferred way to write a space ; the tab character, U+0009
 -}

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
readNumber, readGet, readHash, readChar :: String -> TokenResult

readString s0@('"':s) = parse s "" where
  parse :: String -> String -> TokenResult
  parseEscape :: String -> String -> TokenResult

  parse [] _ = throwError "Impossible."
  parse "\"" !ss = return $ StringLiteral $ reverse ss
  parse ('\\':cs) !ss = parseEscape cs ss
  parse ('\"':_) _ = throwError "Impossible."
  parse (c:cs) ss = parse cs (c:ss)
  -- TODO escapes

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

readDotOperator _ = error "readDotOperator: Not a dot operator."

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

readChar ['#', '\\', c] = return (CharLiteral c)
readChar ('#':'\\':cs) = maybe (illegalToken $ "Unrecognized char name: " ++ cs)
                               (return . CharLiteral) res where
  res = M.lookup cs charTable `mplus` tryHex
  tryHex = case cs of
             c0:cs' -> if elem c0 "uUxX" && all (`elem` hexDigits) cs'
                       then Just (read $ "'\\x" ++ cs' ++ "'") else Nothing
             _ -> Nothing
readChar s0 = illegalToken "Invalid character literal."

readGet ('#':'^':rest)
  | all (== '^') rest = return $ Get (length rest)
  | otherwise = return $ Get (read rest)

readGet _ = error "readGet: Invalid #^ form."

readHash "#" = return Hash
readHash "#t" = return TrueLiteral
readHash "#f" = return FalseLiteral
readHash "#T" = return TrueLiteral
readHash "#F" = return FalseLiteral
readHash s0@('#':s@(_:_)) | all isDigit s = return $ Indexer (read s)
                          | otherwise = illegalToken ("Invalid #form: " ++ s0)
readHash s0 = illegalToken ("Invalid #form: " ++ s0)

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
