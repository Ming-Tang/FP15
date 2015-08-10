{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}
-- | The @FP15.Compiler@ module contains logic for compiling FP15 programs.
-- The compilation of an FP15 program starts at the source code of the initial
-- module, and ends with the fully-reduced forms of all functions in the
-- initial module and its dependencies. The fully-reduced forms are ready to be
-- processed by interpreters or code generators.
module FP15.Compiler where
import Data.List(intercalate)
import Data.Either
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Maybe(mapMaybe)
import FP15.Types
import FP15.Compiler.Types
import FP15.Compiler.Reduction
import FP15.Evaluator.Types(BaseExpr)
import qualified FP15.Evaluator.Types as E
import qualified FP15.Compiler.ModuleBody as MB
import qualified FP15.Compiler.ImportedNames as IN
--import FP15.Compiler.ModuleResolution()
--import FP15.Compiler.CompiledModuleSet()

data ImportError = CannotImport ModuleName
                 | NameNotInModule String
                 deriving (Eq, Ord, Show, Read)

-- | Given module AST, resolve their names and operators
stageModule :: CompiledModuleSet -> ModuleAST
               -> Either (Located ImportError) ReducingModuleState
stageModule cms mast@ModuleAST { astMN, astFs, astFls
                               , astFFixes, astFlFixes
                               , astImps , astExps } = do
  iN <- resolveImports cms $ addImplicitPrelude astImps
  let ss = SS { ssCMS = cms, ssMN = astMN, ssMI = mi, ssSM = Just sm, ssIN = iN }
      rmod = Reducing Module { fs = mfs, fls = mfls
                             , fFixes = M.empty, flFixes = M.empty }
      mfs = M.map Unresolved $ M.mapKeys getLocated astFs
      mfls = M.mapKeys getLocated astFls
      mi = getModuleInterface mast
      sm = getSourceMapping mast
  return $ ReducingModuleState ss Normal rmod

addImplicitPrelude :: ImportList -> ImportList
addImplicitPrelude = (Loc Nothing (Import $ M ["Std"]):)

getSourceMapping :: ModuleAST -> SourceMapping
getSourceMapping ModuleAST { astFs, astFls, astFFixes, astFlFixes }
  = SourceMapping Module { fs = conv astFs, fls = conv astFls
                         , fFixes = conv astFFixes, flFixes = conv astFlFixes } where
  conv :: Map (LocId i) b -> Map (Id i) SrcPos
  conv = M.fromList . mapMaybe convLocId . M.toList
  convLocId :: (LocId i, x) -> Maybe (Id i, SrcPos)
  convLocId (Loc s i, _) = fmap (\s' -> (i, s')) s

-- | The 'resolveImports' function resolves import declarations into a mapping
-- of imported symbol to absolute symbol, given a 'CompiledModuleSet' and an
-- 'ImportList'.
resolveImports :: CompiledModuleSet -> ImportList
              -> Either (Located ImportError) ImportedNames
resolveImports cms = foldM addImportL $ Imported MB.empty where
  addImportL :: ImportedNames -> Located Import
                -> Either (Located ImportError) ImportedNames
  addImportL ins li@(Loc l i) = makeErrLocated l $ addImport ins li i

  addImport :: ImportedNames -> Located Import -> Import
               -> Either ImportError ImportedNames
  addImport ins li (Import m) = with qualified li m ins >>= with unqualified li m
  addImport ins li (ImportQualified m) = with qualified li m ins
  addImport ins li _ = error $ "FP15.Compiler.resolveImports: Not implemented: " ++ show li

  (unqualified, qualified) = (const ((,) []), (,))

  with :: ([String] -> String -> ([String], String))
          -> Located Import -> ModuleName -> ImportedNames
          -> Either ImportError ImportedNames
  with q li m ins = do
    mi <- lookupM m
    let iins = IN.miToImportedNames q li m mi
    return $ iins `IN.mergeImportedNames` ins

  lookupM :: ModuleName -> Either ImportError ModuleInterface
  lookupM m = case M.lookup m (getCompiledModuleSet cms) of
                Nothing -> throwError $ CannotImport m
                Just (CompiledModuleItem _ mi _) -> return mi

  makeErrLocated l (Right x) = Right x
  makeErrLocated l (Left e) = Left (Loc l e)

-- | The 'getModuleInterface' function determines a module's public interface
-- given a 'ModuleAST'.
getModuleInterface :: ModuleAST -> ModuleInterface
getModuleInterface ModuleAST { astFs, astFls
                             , astFFixes, astFlFixes
                             , astImps, astExps }
  = ModuleInterface Module { fs = miFs, fls = miFls
                           , fFixes = miFFxs, flFixes = miFlFxs } where
    extract = M.map cu . extract'
    extract' = M.mapKeys getLocated
    extract :: Map (Located (Id b)) a -> Map (Id b) ()
    extract' :: Map (Located (Id b)) a -> Map (Id b) a
    (miFs, miFls) = (extract astFs, extract astFls)
    (miFFxs, miFlFxs) = (extract' astFFixes, extract' astFlFixes)
    cu = const ()
    -- TODO check for presence of Export

-- | The 'stepModule' function performs one reduction step of a module.
stepModule :: ReducingModuleState -> Either String ReducingModuleState
stepModule r@(ReducingModuleState ss rt (Reducing rm@Module { fs })) =
  case es of
    [] -> return (ReducingModuleState ss rt' (Reducing rm { fs = us }))
    _ -> throwError $ intercalate "\n" $ map (\(a, b) -> show a ++ ": " ++ fromLeft b) es
  where
    fs' = M.map mm fs
    us = M.map fromRight fs'
    es = filter (\(_, x) -> isLeft x) $ M.toList fs'

    ff = M.foldr (\a b -> case a of Reduced _ -> b
                                    _ -> False) True us
    rt' = if ff then Finished else rt

    mm :: ExprState -> Either String ExprState
    mm (Unresolved x) = ce $ Unlifted <$> convExprAST ss x
    mm (Unlifted x) = ce $ Unreduced <$> liftBExpr x
    mm (Unreduced x) = ce $ Reduced <$> resolveExpr ss x
    mm x = return x

    ce :: Show a => Either a b -> Either String b
    ce (Left x) = Left $ show x
    ce (Right y) = Right y

    fromRight (Right x) = x
    fromRight (Left _) = error "fromRight"

    fromLeft (Left x) = x
    fromLeft (Right _) = error "fromLeft"

-- | Given a 'ReducingModuleState', convert it to an 'CompiledModuleItem'.
--
-- Precondition: the module is in 'Finished' state and all expressions are
-- reduced.
makeCompiledModule :: ReducingModuleState -> CompiledModuleItem
makeCompiledModule (ReducingModuleState SS { ssMI = mi, ssSM = sm }
                     tg (Reducing Module { fs = mfs, fls = mfls
                                         , fFixes = mffxs, flFixes = mflfxs }))
  = chkFinished `seq` CompiledModuleItem cm mi sm where
  cm = Compiled Module { fs = M.map gRE mfs
                       , fls = mfls
                       , fFixes = mffxs
                       , flFixes = mflfxs }
  gRE (Reduced e) = e
  gRE _ = error "makeCompiledModule: An expr is not reduced."
  chkFinished = if tg == Finished then ()
                else error "makeCompiledModule: Module is not reduced."

translateCMS :: CompiledModuleSet -> Either (Map E.Ident BaseExprError)
                                            (Map E.Ident BaseExpr)
translateCMS (flattenCMS -> fd)
  = if M.null es then Right bs
    else Left es where
  m = M.mapKeys disp $ M.mapWithKey (\k e -> E.Mark (disp k) <$> e) $ M.map toBaseExpr fd
  (M.fromList -> es0, M.fromList -> bs) = partitionEithers $ map h $ M.toList m
  h (a, Left x) = Left (a, x)
  h (a, Right x) = Right (a, x)
  es = M.filter (/= BaseF) es0

flattenCMS :: CompiledModuleSet -> Map FName Expr
flattenCMS (CompiledModuleSet m) = M.fromList $ concatMap f $ M.toList m where
  f (M mn, CompiledModuleItem { cmiCM = Compiled Module { fs } })
    = map (\(Id x, e) -> (N mn x, e)) $ M.toList fs
