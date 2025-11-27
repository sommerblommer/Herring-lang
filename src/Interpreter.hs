module Interpreter where 
import TypedAst 

data Value = Integer | Boolean | Var 

type Env = Env {store :: [(String, Value)]}

evaluateExpression :: Expr -> IO () 
evaluateExpression _ = error "todo"

evaluateStatement :: Stm -> IO ()
evaluateStatement _ = error "todo"

evaluateAST :: TypedAst -> IO ()
evaluateAST _ = error "TODO"



