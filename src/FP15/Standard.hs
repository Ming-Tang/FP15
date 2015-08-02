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
    ("<", (8, 0), VarAssoc, "lt")
  , ("<=", (8, 0), VarAssoc, "le")
  , (">", (8, 0), VarAssoc, "gt")
  , (">=", (8, 0), VarAssoc, "ge")

  , ("+", (9, 0), LeftAssoc, "add")
  , ("-", (9, 0), LeftAssoc, "sub")
  , ("*", (10, 0), LeftAssoc, "mul")
  , ("%", (10, 0), LeftAssoc, "div")
  ]

stdFlFixes = map (\(o, p, a, f) -> (Id o, Fixity a p (stdName f))) [
    ("->", (-2, 0), RightAssoc, "Chain")

  , ("@", (-1, 0), Prefix, "Map")
  , ("@<", (-1, 0), Prefix, "MapL")
  , ("@>", (-1, 0), Prefix, "MapR")

  , ("?", (-1, 0), Prefix, "Filter")
  , ("?<", (-1, 0), Prefix, "FilterL")
  , ("?>", (-1, 0), Prefix, "FilterR")

  , ("/", (3, 0), Prefix, "InsL")
  , ("\\", (3, 0), Prefix, "InsR")
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

