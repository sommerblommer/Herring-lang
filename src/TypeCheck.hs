module TypeCheck where 
import Ast 
import TypedAst as TAST

typeOfFunction :: Ast.Op -> Typ
typeOfFunction Ast.Plus = IntType 
typeOfFunction Ast.Minus = IntType

typeCheckExpr :: Ast.Expr -> (TAST.Expr, TAST.Typ)
typeCheckExpr LitExp {lit = LI i} = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr LitExp {lit = LB i} = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr IExp {ident = str} = (Ident str, StringType)
typeCheckExpr Ast.BinOp {lhs=l, op=operator, rhs=r} = 
    let (leftExp, ltyp) = typeCheckExpr l in
    let (rightExp, rtyp) = typeCheckExpr r in
    let opType = typeOfFunction operator in
    if ltyp == opType && rtyp == opType then 
        (TAST.BinOp leftExp TAST.Plus rightExp opType, opType)
    else 
        error "type mismatch"

typeCheckStm :: Ast.Stm -> TAST.Stm 
typeCheckStm (Ast.LetIn str expr) = 
    let (texpr, exprtype) = typeCheckExpr expr in
    TAST.LetIn str texpr exprtype
typeCheckStm (Ast.Scope stms) = 
    TAST.Scope (typeCheckStm <$> stms)
typeCheckStm (Ast.Return expr) = 
    let (texpr, exprtype) = typeCheckExpr expr in
    TAST.Return texpr exprtype
typeCheckStm (Ast.Exp expr) =
    let (texpr, exprtype) = typeCheckExpr expr in
    TAST.StmExpr texpr exprtype

