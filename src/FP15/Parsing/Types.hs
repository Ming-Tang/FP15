module FP15.Parsing.Types where
import FP15.Types

data TokenData
  = EOF
  | Illegal !String

-- Identifiers

  | Function FName
  | Functional FlName
  | Operator UnknownName
  | DotOperator FName

-- Literals

  | FalseLiteral
  | TrueLiteral
  | CharLiteral !Char
  | IntLiteral !Integer
  | RealLiteral !Double
  | SymbolLiteral !String
  | StringLiteral !String
  | Indexer !Int

-- Operators

  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | LParen
  | RParen

  | Pipe
  | Colon
  | Comma
  | Semicolon
  deriving (Eq, Ord, Show, Read)

data Token = Token !TokenData !SrcPos !String
           deriving (Eq, Ord, Show, Read)

data StateTag = Offside | Bracket | Paren | Brace
              deriving (Eq, Ord, Show, Read)

data StateItem = StateItem { stateTag :: !StateTag
                           , indent :: !Int }
               deriving (Eq, Ord, Show, Read)

type StateStack = [StateItem]

data State = Normal { stateStack :: !StateStack
                    , pendingIndent :: !(Maybe Int)
                    , currentFile :: !(Maybe String) }
           | BlockComment { nestingLevel :: !Int
                          , stateStack :: !StateStack
                          , pendingIndent :: !(Maybe Int)
                          , currentFile :: !(Maybe String) }
           deriving (Eq, Ord, Show, Read)
