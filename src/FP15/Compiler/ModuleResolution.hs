module FP15.Compiler.ModuleResolution where
import Control.Applicative((<$>))
import qualified Data.Map.Strict as M
import System.Directory(doesFileExist)
import System.FilePath.Posix(joinPath, (</>))
import FP15.Types
import Control.Monad.Identity

-- | A @Resolver@ retrieves the source code and filename (if relevant) given a
-- module name. If the resolution succeeds, @Just@ of the source code of the
-- module is returned,otherwise, @Nothing@ is returned to indicate the module
-- cannot be resolved. A @Resolver@ operates under a monad @m@ because some
-- module resolvers require @IO@ (for example, reading files) while other module
-- resolvers are pure (for example, testing data).
type Resolver m = ModuleName -> m (Maybe ModuleSource)

runForSource :: Identity (Maybe ModuleSource) -> Maybe String
runForSource m = moduleSource <$> runIdentity m

-- | The 'nullResolver' cannot resolve any module at all.
--
-- >>> runForSource $ nullResolver $ ModuleName ["A"]
-- Nothing
-- >>> runForSource $ nullResolver $ ModuleName ["A", "B"]
-- Nothing
nullResolver :: Monad m => Resolver m
nullResolver _ = return Nothing

-- | The 'constResolver' returns the same source code for any given module.
--
-- >>> runForSource $ constResolver "Source" $ ModuleName ["A"]
-- Just "Source"
-- >>> runForSource $ constResolver "Source" $ ModuleName ["A", "B"]
-- Just "Source"
constResolver :: Monad m => String -> Resolver m
constResolver s _ = return $ Just (ModuleSource Nothing s)

-- | The 'mapResolver' looks up the module's source code from a 'Map'.
--
-- >>> runIdentity $ mapResolver M.empty $ ModuleName ["A", "B"]
-- Nothing
-- >>> :{
--     let moduleMap = M.fromList [(ModuleName ["A", "B"], "<src for A.B>"),
--                                 (ModuleName ["C", "D"], "<src for C.D>")]
--     :}
--
-- >>> runForSource $ mapResolver moduleMap $ ModuleName ["A", "B"]
-- Just "<src for A.B>"
-- >>> runForSource $ mapResolver moduleMap $ ModuleName ["C", "D"]
-- Just "<src for C.D>"
-- >>> runForSource $ mapResolver moduleMap $ ModuleName ["E", "F"]
-- Nothing
-- >>> runForSource $ mapResolver moduleMap $ ModuleName ["A"]
-- Nothing
mapResolver :: Map ModuleName String -> Resolver Identity
mapResolver mm = resolver $ flip M.lookup mm >=> (return . ModuleSource Nothing)

-- | The 'resolver' function creates a module resolver of 'Identity' monad from
-- a pure function.
--
-- >>> runForSource $ resolver (\m -> Just $ ModuleSource Nothing $ show $ length $ getModuleName m) $ ModuleName ["A", "B"]
-- Just "2"
-- >>> runForSource $ resolver (\m -> if getModuleName m == ["X"] then Just $ ModuleSource Nothing "X" else Nothing) $ ModuleName ["A", "B"]
-- Nothing
-- >>> runForSource $ resolver (\m -> if getModuleName m == ["X"] then Just $ ModuleSource Nothing "X" else Nothing) $ ModuleName ["X"]
-- Just "X"
resolver :: (ModuleName -> Maybe ModuleSource) -> Resolver Identity
resolver = (return .)

-- | The 'pathResolver' resolves modules by looking up modules of base paths from
-- the specified paths in the filesystem. Relative paths are relative to the
-- working directory.
--
-- For example, @pathResolver [".", ".\/src", "\/usr\/share\/fp15"]@
-- would look up the module @A.B@ from paths @".\/A\/B"@, @".\/src\/A\/B"@ and
-- @"\/usr\/share\/fp15\/A\/B"@.
--
-- >>> pathResolver ["."] $ ModuleName ["FP15","Compiler.hs"]
-- Nothing
-- >>> pathResolver [".","src"] $ ModuleName ["FP15","Compiler.hs"]
-- Just ...
-- >>> pathResolver [".","src"] $ ModuleName ["FP15","DoesNotExist.hs"]
-- Nothing
pathResolver :: [FilePath] -> Resolver IO
pathResolver paths m = loop searchPaths
  where modulePath = joinPath $ getModuleName m
        searchPaths = map (</> modulePath) paths
        loop [] = return Nothing
        loop (p:ps) = do e <- doesFileExist p
                         if e then result p <$> readFile p
                         else loop ps
        result p src = length src `seq` Just (ModuleSource (Just p) src)

