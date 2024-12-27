module TypeCheck where 
import Ast 
import TypedAst as TAST
import Data.Map

typeOfFunction :: Ast.Op -> Typ
typeOfFunction Ast.Plus = IntType 
typeOfFunction Ast.Minus = IntType

newtype Env = Env {vars :: Map String Typ}
emptEnv :: Env 
emptEnv = Env {vars = empty}

insertIntoEnv :: String -> Typ -> Env -> Env 
insertIntoEnv s t e = Env {vars = insert s t $ vars e}

typeCheckExpr :: Env -> Ast.Expr -> (TAST.Expr, TAST.Typ)
typeCheckExpr _ LitExp {lit = LI i} = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr _ LitExp {lit = LB i} = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr env IExp {ident = str} = 
    let lup = vars env !? str in 
    case lup of 
        Just t -> (Ident str, t)
        Nothing -> error $ "variable: " ++ show lup ++ " has not defined"
typeCheckExpr env Ast.BinOp {lhs=l, op=operator, rhs=r} = 
    let (leftExp, ltyp) = typeCheckExpr env l in
    let (rightExp, rtyp) = typeCheckExpr env r in
    let opType = typeOfFunction operator in
    if ltyp == opType && rtyp == opType then 
        (TAST.BinOp leftExp TAST.Plus rightExp opType, opType)
    else 
        error "type mismatch"

typeCheckStm :: Env -> Ast.Stm -> (TAST.Stm, Env)
typeCheckStm env (Ast.LetIn str expr) = 
    let (texpr, exprtype) = typeCheckExpr env expr in
    let newenv = insertIntoEnv str exprtype env in 
    (TAST.LetIn str texpr exprtype, newenv)
typeCheckStm env (Ast.Scope stms) = 
    let (res, env') =  Prelude.foldl (\(acc, env) x -> 
                                    let (stm, newenv) = typeCheckStm env x in
                                    (stm : acc, newenv)
                                    ) ([], emptEnv) stms
    in 
    (TAST.Scope (reverse res), env')
typeCheckStm env (Ast.Return expr) = 
    let (texpr, exprtype) = typeCheckExpr env expr in
    (TAST.Return texpr exprtype, env)
typeCheckStm env (Ast.Exp expr) =
    let (texpr, exprtype) = typeCheckExpr env expr in
    (TAST.StmExpr texpr exprtype, env)

typeCheckAst :: Ast.Ast -> TAST.Ast
typeCheckAst stms =
    reverse . fst $ Prelude.foldl (\(acc, env) x -> 
                                    let (stm, newenv) = typeCheckStm env x in
                                    (stm : acc, newenv)
                                    ) ([], emptEnv) stms

