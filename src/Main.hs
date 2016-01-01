{-# LANGUAGE Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Control.DeepSeq(force)
import Control.Applicative((<$>))
import Control.Monad.Error()
import Data.Map((!))
import Text.PrettyPrint()
import FP15.Parsing()
import FP15.Parsing.Types
import FP15.Parsing.Lexer(scanTokens)
import FP15.Parsing.Parser(parse)
import FP15.Value
import FP15.Types hiding (Const, Func)
import FP15.Evaluator()
import FP15.Evaluator.Types
import FP15.Evaluator.Contract()
import FP15.Compiler
import FP15.Compiler.Types
import FP15.Compiler.CompiledModuleSet
import FP15.Compiler.PrettyPrinting()
import FP15.Compiler.Syntax()
import FP15.Standard(standardCMS)
import FP15.Evaluator.Standard(standardEnv)
import FP15.Evaluator.Translation(transMap)
import FP15.Evaluator.FP(execFP)

main :: IO ()
{-
import Data.Map((!))
import FP15.Evaluator.Standard(standardEnv)
import FP15.Evaluator.Translation(BaseExpr(..), transMap)

interactLines :: (String -> String) -> IO ()
interactLines l = interact (unlines . map l . lines)

valueOrError (Left e) = error e
valueOrError (Right x) = x

printTokens :: String -> IO ()
printTokens = putStrLn . unlines . map show . valueOrError . scanTokens

compile :: String -> IO ()
compile s = moduleResolution resolve [ModuleName ["Main"]] >>= print
  where resolve (ModuleName ["Main"]) = return $ Just (ModuleSource Nothing s)
        resolve m = pathResolver ["."] m

main = getContents >>= compile
-}

main = do
  src <- getContents
  let ast = unwrap $ parse $ ModuleSource Nothing src
  let m = unwrap $ stageModule standardCMS ast
  let m' = force $ until ((==) Finished . rmsTag) (unwrap . stepModule) m
  let c = force $ makeCompiledModule m'
  let cms' = force $ addModule (ssMN $ rmsSS m') c standardCMS
  let (CompiledModuleSet cmis) = force cms'
  -- print $ vcat $ prettyCMILines (M ["Main"]) (cmis ! (M ["Main"]))
  let fl = unwrap $ translateCMS cms'
  let s = force $ transMap standardEnv fl
  res <- fmap unwrap $ execFP $ (s ! "Main.main") (Extended $ RealWorld RW)
  putStrLn $ disp (res :: FPValue)
  where unwrap (Left x) = error $ disp x
        unwrap (Right x) = force x
{-
      case m of
        Left e -> print e
        Right r@(ReducingModuleState SS { ssIN = in_ }
                                     _ (Reducing Module { fs })) ->
          let fs' = M.map mm fs
              mm (Unresolved n) = convExprAST in_ n
              mm x = error (show x) in
          print (fs, fs')
-}

-- main = getContents >>= printTokens

-- $testSplitTokens
-- >>> splitTokens "abc def"
-- Right ["abc","def"]
-- >>> splitTokens "abc.def"
-- Right ["abc",".def"]
-- >>> splitTokens "Abc.def"
-- Right ["Abc.def"]
-- >>> splitTokens "abc!!def"
-- Right ["abc!","!","def"]
-- >>> splitTokens "abc+.def"
-- Right ["abc","+",".def"]
-- >>> splitTokens "abc?def"
-- Right ["abc?","def"]
-- >>> splitTokens "abc??def"
-- Right ["abc?","?","def"]
-- >>> splitTokens "X.Y...z"
-- Right ["X.Y","...","z"]
-- >>> splitTokens "Abc.Def.Ghi.jkl.Mno.Pqr.Stu."
-- Right ["Abc.Def.Ghi.jkl",".","Mno.Pqr.Stu","."]
-- >>> splitTokens "Abc.Def.Ghi.jkl.Mno.Pqr.Stu.(++)"
-- Right ["Abc.Def.Ghi.jkl",".","Mno.Pqr.Stu.(++)"]
-- >>> splitTokens "Abc.Def.Ghi.jkl.Mno.Pqr.Stu.f"
-- Right ["Abc.Def.Ghi.jkl",".Mno.Pqr.Stu.f"]
-- >>> splitTokens "!<+!<<"
-- Right ["!<+","!<<"]
-- >>> splitTokens "@@@!"
-- Right ["@","@","@","!"]

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Monad
-- >>> import Test.QuickCheck
-- >>> newtype T = T String deriving Show
-- >>> let genChar = elements $ ['a'..'z']++['A'..'Z']++"~!@#$%^&*()_+`-={}|[]\\:;<>?,./"
-- >>> instance Arbitrary T where arbitrary = T <$> (listOf genChar)

-- $props
-- prop> \(T s) -> splitTokens (s :: String) == (splitTokens =<< (fmap (concatMap (++ " ")) $ splitTokens s))

-- | The 'splitTokens' function splits a string into tokens.
splitTokens :: String -> Either String [String]
splitTokens s = (map (\(Token _ _ str) -> str) . init) <$> scanTokens s
