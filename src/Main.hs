module Main where
import Data.Map((!))
import qualified Data.Map as M
import FP15.Value
import FP15.Types()
import FP15.Evaluator()
import FP15.Evaluator.Contract()
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

present (Left x) = print x >> putStrLn ""
present (Right x) = print x >> putStrLn ""

main :: IO ()
main = do
  present fac25
  present facSym
  present mf1
  present mf2
  present mf3
  return ()
