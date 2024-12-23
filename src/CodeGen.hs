module CodeGen where 
import TypedAst

type Reg = Int
data Operand = Lit Int | Str String | R Reg

data Operation = Mov | Svc | Adr

data CodeLine = CL Operation Operand Operand

codegenExpr :: [Reg] -> Expr -> CodeLine 
codegenExpr (fr:_) (Literal {tLit=TLI i}) = CL Mov (R fr) (Lit i) 
codegenExpr (fr:_) (Literal {tLit=TLB b}) 
    | b =  CL Mov (R fr) (Lit 1) 
    | otherwise = CL Mov (R fr) (Lit 0) 
codegenExpr _ e = error $ "expression not yet implemented: " ++ show e
