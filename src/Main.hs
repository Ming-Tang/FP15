{-# LANGUAGE Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import qualified Data.Map as M
import Data.Map((!))
import FP15.Parsing()
import FP15.Parsing.Types
import FP15.Parsing.Lexer(scanTokens)
import FP15.Parsing.Parser(parse)
import FP15.Value
import FP15.Types hiding (Const, Func)
import FP15.Evaluator()
import FP15.Evaluator.Contract()
import FP15.Compiler
import FP15.Compiler.Types
import FP15.Compiler.CompiledModuleSet
import FP15.Compiler.Precedence()
import FP15.Compiler.Reduction()
import FP15.Standard(standardCMS)
import FP15.Evaluator.Standard(standardEnv)
import FP15.Evaluator.Translation(transMap)

main :: IO ()
{-
import Data.Map((!))
import FP15.Evaluator.Standard(standardEnv)
import FP15.Evaluator.Translation(BaseExpr(..), transMap)

(c, f, fo, cm) = (Const, Func, Fork, Compose)

fac, mf :: BaseExpr
fac = If (f "zero?")
         (c (Int 1))
         (cm [
           fo [f "i",
               cm [f "pred", f "fac"]],
           f "*"])

mf = cm [Map (f "succ"), Filter (f "even?")]

env = transMap standardEnv $ M.fromList [("fac", fac), ("mf", mf)]

fac25 = (env ! "fac") (Int 25)
facSym = (env ! "fac") (Symbol "test")
mf1 = (env ! "mf") (String "Hello, world!")
mf2 = (env ! "mf") (List [Int 1, Int 3, Int 2])
mf3 = (env ! "mf") (List [Int 1, Real 15.0, Bool False, Real 3.0])

main'' = do
  present fac25
  present facSym
  present mf1
  present mf2
  present mf3
  return ()

present (Left x) = print x >> putStrLn ""
present (Right x) = print x >> putStrLn ""

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
  let m = unwrap' $ stageModule standardCMS ast
  let m' = until ((==) Finished . rmsTag) (unwrap . stepModule) m
  let c = makeCompiledModule m'
  let cms' = addModule (ssMN $ rmsSS m') c standardCMS
  let fl = unwrap' $ translateCMS cms'
  print fl
  let s = transMap standardEnv fl
  print (M.keys s)
  print $ (s ! "Main.main") (List [])
  where unwrap (Left x) = error x
        unwrap (Right x) = x
        unwrap' (Left x) = error (show x)
        unwrap' (Right x) = x
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
splitTokens s = scanTokens s >>= return . map (\(Token _ _ str) -> str) . init
