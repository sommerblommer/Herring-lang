module Ast  where 

data Lit = LI Int | LB Bool 

data Op = Plus | Minus 

data Expr = 
    LitExp {lit :: Lit}
    | IExp {ident :: String}
    | BinOp {lhs :: Expr, op :: Op, rhs :: Expr}

data Stm = 
    LetIn String Expr 
    | Return Expr
    | Scope [Stm]
    | Exp Expr

instance Show Op where 
    show Plus = "+"
    show Minus = "-"

instance Show Expr where
    show IExp {ident=s} = s 
    show LitExp {lit=LI i} =  show i
    show LitExp {lit=LB b} =  show b
    show BinOp {lhs=l, op=o, rhs=r} = "(" ++ show l ++ show o ++ show r ++ ")"

instance Show Stm where 
    show (LetIn str expr) = "let " ++ str ++ " = " ++ show expr ++ " in"
    show (Return e) = "return " ++ show e
    show (Scope stms) = foldl (\acc s -> acc ++ "\n" ++ show s) "" stms
    show (Exp ex) = show ex


