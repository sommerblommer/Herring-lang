module TypedAst where 

data Typ = IntType | BoolType | StringType
    deriving (Eq, Show)

data Lit = TLI Int | TLB Bool
    deriving (Show)

data Op = Plus | Minus
    deriving (Show)

data Expr = 
    Literal {tLit :: Lit} 
    | Ident String 
    | BinOp Expr Op Expr Typ
    deriving (Show)

data Stm = 
    LetIn String Expr Typ
    | Scope [Stm] 
    | Return Expr Typ
    | StmExpr Expr Typ

type Ast = [Stm]
