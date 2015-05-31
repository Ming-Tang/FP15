module FP15.AST where
import FP15.Value(Value)

data ExprAST = TValue Value
             | TFunc UnresolvedFunction
             | TApp UnresolvedFunctional [ExprAST]
             | TUnary UnresolvedFunctional ExprAST
             | TBinary ExprAST UnresolvedFunctional ExprAST

             | TLeftSection UnresolvedFunction ExprAST
             | TMiddleSection ExprAST UnresolvedFunction ExprAST
             | TRightSection ExprAST UnresolvedFunction

             | TIf ExprAST ExprAST ExprAST
             | TFilter1 ExprAST ExprAST
             | TFilter ExprAST ExprAST ExprAST
             | TFork [ExprAST]
             | TPass [ExprAST]
             deriving (Eq, Show, Read)

