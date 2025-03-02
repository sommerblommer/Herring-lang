module TypeCheck where 
import Ast 
import TypedAst as TAST
import Data.Map as DM
import Data.List 

typeOfFunction :: Ast.Op -> Typ
typeOfFunction Ast.Plus = IntType 
typeOfFunction Ast.Mult = IntType 
typeOfFunction Ast.Minus = IntType

astOpToTASTOp :: Ast.Op -> TAST.Op
astOpToTASTOp Ast.Plus = TAST.Plus 
astOpToTASTOp Ast.Mult = TAST.Mult 
astOpToTASTOp Ast.Minus = TAST.Minus

data Env = Env {vars :: Map String Typ, functions :: [TAST.Function]}
emptEnv :: Env 
emptEnv = Env {vars = empty, functions = []}

insertIntoEnv :: String -> Typ -> Env -> Env 
insertIntoEnv s t e = e {vars = DM.insert s t $ vars e}

lookupFunc ::  Env -> String -> TAST.Function 
lookupFunc env iden = case find (\a -> TAST.funName a == iden) (functions env) of 
    Just function -> function
    Nothing -> case find (\f -> TAST.funName f == iden) predefinedFunctions of
                    Just f -> f
                    Nothing -> error "function as variables not supported yet" 

getTypeOfExpr :: Env -> TAST.Expr -> Typ 
getTypeOfExpr env (Ident var) = case vars env DM.!? var  of 
    Just t -> t 
    Nothing -> error $ show var ++ " has no type"
getTypeOfExpr _ Literal {tLit=l} = case l of 
    (TLI _) -> IntType 
    (TLB _) -> BoolType
getTypeOfExpr _ (TAST.BinOp _ _ _ t) = t
getTypeOfExpr _ (TAST.FunCall _ _ t) = t

typeCheckExpr :: Env -> Ast.Expr -> (TAST.Expr, TAST.Typ)
typeCheckExpr _ LitExp {lit = LI i} = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr _ LitExp {lit = LB i} = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr env IExp {ident = str} = 
    let lup = vars env !? str in 
    case lup of 
        Just t -> (Ident str, t)
        Nothing -> error $ "variable: " ++ str ++ " has not been defined"
typeCheckExpr env Ast.BinOp {lhs=l, op=operator, rhs=r} = 
    let (leftExp, ltyp) = typeCheckExpr env l in
    let (rightExp, rtyp) = typeCheckExpr env r in
    let opType = typeOfFunction operator in
    if ltyp == opType && rtyp == opType then 
        (TAST.BinOp leftExp (astOpToTASTOp operator) rightExp opType, opType)
    else 
        error "type mismatch"
typeCheckExpr env (Ast.FunCall fvar args) = 
    let (typedFvar, fname) = case fvar of 
            (IExp {ident = ide} ) -> (lookupFunc env ide, ide)
            _ -> error $ show fvar ++ " not a valid function call"
    in
    let typedArgs = Data.List.map (\(arg, (_, pType)) -> 
                let (typeArg, typ) = typeCheckExpr env arg in
                if pType == typ  then 
                    typeArg else error $ show fvar ++ " is called with wrong types"
            ) $ zip args $ TAST.params typedFvar
    in
    let rType = TAST.returnType typedFvar in
    (TAST.FunCall (Ident fname) typedArgs rType, rType) 
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

