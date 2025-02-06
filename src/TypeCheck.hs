module TypeCheck where 
import Ast 
import TypedAst as TAST
import Data.Map
import Data.List 

typeOfFunction :: Ast.Op -> Typ
typeOfFunction Ast.Plus = IntType 
typeOfFunction Ast.Minus = IntType

data Env = Env {vars :: Map String Typ, functions :: [TAST.Function]}
emptEnv :: Env 
emptEnv = Env {vars = empty, functions = []}

insertIntoEnv :: String -> Typ -> Env -> Env 
insertIntoEnv s t e = e {vars = Data.Map.insert s t $ vars e}

lookupFunc ::  Env -> String -> TAST.Expr 
lookupFunc env ident = case find (\a -> TAST.funName a == ident) (functions env) of 
    Just _ -> TAST.Ident ident
    Nothing -> case Data.Map.lookup ident (vars env) of 
        Just _ -> TAST.Ident ident 
        Nothing -> error $ ident ++ " not a function"

typeCheckExpr :: Env -> Ast.Expr -> (TAST.Expr, TAST.Typ)
typeCheckExpr _ LitExp {lit = LI i} = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr _ LitExp {lit = LB i} = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr env IExp {ident = str} = 
    let lup = vars env !? str in 
    case lup of 
        Just t -> (Ident str, t)
        Nothing -> error $ "variable: " ++ str ++ " has not defined"
typeCheckExpr env Ast.BinOp {lhs=l, op=operator, rhs=r} = 
    let (leftExp, ltyp) = typeCheckExpr env l in
    let (rightExp, rtyp) = typeCheckExpr env r in
    let opType = typeOfFunction operator in
    if ltyp == opType && rtyp == opType then 
        (TAST.BinOp leftExp TAST.Plus rightExp opType, opType)
    else 
        error "type mismatch"
typeCheckExpr env (Ast.FunCall fvar params) = 
    let typedFvar = case fvar of 
            (IExp {ident = ide} ) -> lookupFunc env ide  
            _ -> error $ show fvar ++ " not a valid function call"
    in
    
    error ""
typeCheckExpr _ e = error $ show e ++ " not impplemented"

typeCheckStm :: Env -> Ast.Stm -> (TAST.Stm, Env)
typeCheckStm env (Ast.LetIn str expr) = 
    let (texpr, exprtype) = typeCheckExpr env expr in
    let newenv = insertIntoEnv str exprtype env in 
    (TAST.LetIn str texpr exprtype, newenv)
typeCheckStm env (Ast.Scope stms) = 
    let (res, env'') =  Prelude.foldl (\(acc, env') x -> 
                                    let (stm, newenv) = typeCheckStm env' x in
                                    (stm : acc, newenv)
                                    ) ([], env) stms
    in 
    (TAST.Scope (reverse res), env'')
typeCheckStm env (Ast.Return expr) = 
    let (texpr, exprtype) = typeCheckExpr env expr in
    (TAST.Return texpr exprtype, env)
typeCheckStm env (Ast.Exp expr) =
    let (texpr, exprtype) = typeCheckExpr env expr in
    (TAST.StmExpr texpr exprtype, env)

typeCheckFunction :: Env ->  Ast.Function -> (TAST.Function, Env)
typeCheckFunction env func = 
    let pms = typeCheckParams (Ast.params func) in
    let newenv = Prelude.foldr (\(str, typ) acc -> insertIntoEnv str typ acc) env pms in 
    let (newbody, env') = typeCheckStm newenv $ Ast.body func in
    let tfunc = (TAST.Function {TAST.funName = Ast.funName func, TAST.params = pms, TAST.body = newbody, TAST.returnType = IntType}) in
    let env'' = env {functions = tfunc : functions env'} in
    (tfunc, env'')
        where 
        typeCheckParams :: [(String, String)] -> [(String, Typ)]
        typeCheckParams xs = do x <- xs 
                                case x of 
                                    (a, "Int") -> return (a, IntType)
                                    (b, "Bool") -> return (b, BoolType)
                                    (_, unknown) -> error $ "Type: " ++ unknown ++ " is not recognised"


typeCheckAst :: Ast.Ast -> TypedAst
typeCheckAst funs =
    reverse . fst $ Prelude.foldl (\(acc, env) x -> 
                                    let (stm, newenv) = typeCheckFunction env x in
                                    (stm : acc, newenv)
                                    ) ([], emptEnv) funs

