module FP15.Standard where
import qualified Data.Map.Strict as M
import FP15.Types
import FP15.Compiler.Types

stdName :: String -> Name a
stdName = N ["Std"]

stdFs :: [Id F]
stdFls :: [Id Fl]
stdFFixes :: [(Id FOp, Fixity F)]
stdFlFixes :: [(Id FlOp, Fixity Fl)]

stdFs = map Id ["_", "succ", "pred", "isEven", "isOdd"]
stdFls = map Id ["Compose", "If", "Fork", "Pass"]
stdFFixes = map (\(o, p, a, f) -> (Id o, Fixity a p (stdName f))) [
   ("&.", (0, 0), LeftAssoc, "apply")
  , ("++", (0, 0), VarAssoc, "append")
  , ("+++", (0, 0), VarAssoc, "strAppend")
  , ("><", (1, 0), VarAssoc, "cross")
  , ("<>", (1, 0), VarAssoc, "zip")
  , ("<*", (2, 0), RightAssoc, "distl")
  , ("*>", (2, 0), LeftAssoc, "distr")
  , ("&&", (3, 0), VarAssoc, "cons")

  -- Logical
  , ("\\!/", (5, 0), LeftAssoc, "xor")
  , ("\\/", (5, 0), LeftAssoc, "or")
  , ("/\\", (6, 0), LeftAssoc, "and")
  , ("!", (7, 0), Prefix, "not")

  -- Comparison
  , ("=", (8, 0), VarAssoc, "eq")
  , ("!=", (8, 0), RightAssoc, "ne")
  , ("=.", (8, 0), VarAssoc, "neq")
  , ("!=.", (8, 0), VarAssoc, "nne")
  , ("<", (8, 0), VarAssoc, "lt")
  , ("<=", (8, 0), VarAssoc, "le")
  , (">", (8, 0), VarAssoc, "gt")
  , (">=", (8, 0), VarAssoc, "ge")
  , ("<=>", (8, 0), LeftAssoc, "cmp")
  , ("<-", (8, 0), LeftAssoc, "elem")

  -- Range
  , ("..", (8, 0), LeftAssoc, "range")
  , ("..<", (8, 0), LeftAssoc, "xrange")

  -- Arithmetic
  , ("+", (9, 0), LeftAssoc, "add")
  , ("-", (9, 0), LeftAssoc, "sub")
  , ("~", (10, 0), Prefix, "neg")
  , ("*", (10, 0), LeftAssoc, "mul")
  , ("%", (10, 0), LeftAssoc, "div")
  , ("%%", (10, 0), LeftAssoc, "mod")
  , ("%.", (10, 0), LeftAssoc, "idiv")
  , ("~%", (11, 0), Prefix, "reci")
  , ("~^", (11, 0), Prefix, "exp")
  , ("^", (11, 0), RightAssoc, "pow")
  , ("^.", (11, 0), RightAssoc, "ipow")
  , ("!!", (12, 0), RightAssoc, "idx")
  ]

stdFlFixes = map (\(o, p, a, f) -> (Id o, Fixity a p (stdName f))) [
  -- Positive prec means tighter than Compose
   ("@", (1, 0), Prefix, "Map")
  , ("@<", (1, 0), Prefix, "Map")
  , ("@>", (1, 0), Prefix, "Map")
  , ("?", (1, 0), Prefix, "Filter")
  , ("?<", (1, 0), Prefix, "FilterL")
  , ("?>", (1, 0), Prefix, "FilterR")
  , ("/", (3, 0), Prefix, "InsL")
  , ("/<", (3, 0), Prefix, "InsLL")
  , ("/>", (3, 0), Prefix, "InsLR")
  , ("\\", (3, 0), Prefix, "InsR")
  , ("\\<", (3, 0), Prefix, "InsRL")
  , ("\\>", (3, 0), Prefix, "InsRR")
  , ("&", (5, 0), Prefix, "FuncRef")
  ]

stdOpLookup :: Map (Name Unknown) (Either FFixity FlFixity)
stdOpLookup = M.fromList (map mf stdFFixes)
              `M.union` M.fromList (map mfl stdFlFixes) where
  mf (Id i, f) = (N [] i, Left f)
  mfl (Id i, f) = (N [] i, Right f)

set :: Ord a => [a] -> Map a ()
set = M.fromList . map (\n -> (n, ()))

stdMI :: ModuleInterface
stdMI = ModuleInterface Module {
          fs = set stdFs
        , fls = set stdFls
        , fFixes = M.fromList stdFFixes
        , flFixes = M.fromList stdFlFixes
        }

stdCompiledModule :: CompiledModule
stdCompiledModule = Compiled Module {
                      fs = M.empty
                    , fls = M.empty
                    , fFixes = M.fromList stdFFixes
                    , flFixes = M.fromList stdFlFixes
                    }

stdModuleItem :: CompiledModuleItem
stdModuleItem = CompiledModuleItem stdCompiledModule stdMI Nothing

standardCMS :: CompiledModuleSet
standardCMS = CompiledModuleSet (M.fromList [(M ["Std"], stdModuleItem)])

