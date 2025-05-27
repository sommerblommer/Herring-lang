module TypeCheck where 
import Ast 
import TypedAst as TAST
import qualified Data.Map as DM
import Data.List 
import Control.Exception (throw)
import Exceptions 

typeOfFunction :: Ast.Op -> Typ
typeOfFunction Ast.Plus = IntType 
typeOfFunction Ast.Mult = IntType 
typeOfFunction Ast.Minus = IntType 
typeOfFunction Ast.Div = IntType
typeOfFunction Ast.Lt = BoolType 
typeOfFunction Ast.Gt = BoolType 
typeOfFunction Ast.Lte = BoolType
typeOfFunction Ast.Gte = BoolType 
typeOfFunction Ast.Eq = BoolType 

astOpToTASTOp :: Ast.Op -> TAST.Op
astOpToTASTOp Ast.Plus = TAST.Plus 
astOpToTASTOp Ast.Mult = TAST.Mult 
astOpToTASTOp Ast.Minus = TAST.Minus
astOpToTASTOp Ast.Div = TAST.Div
astOpToTASTOp Ast.Lt = TAST.Lt 
astOpToTASTOp Ast.Gt = TAST.Gt 
astOpToTASTOp Ast.Lte = TAST.Lte
astOpToTASTOp Ast.Gte = TAST.Gte 
astOpToTASTOp Ast.Eq = TAST.Eq 

data Env = Env {vars :: DM.Map String Typ, functions :: [TAST.Function]}
emptEnv :: Env 
emptEnv = Env {vars = DM.empty, functions = []}

insertIntoEnv :: String -> Typ -> Env -> Env 
insertIntoEnv s t e = e {vars = DM.insert s t $ vars e}

lookupFunc ::  Env -> String -> (String, [Typ]) 
lookupFunc env iden = 
    let lup = vars env DM.!? iden 
    in case lup of 
            (Just (FunType x)) -> x 
            Nothing -> case find (\(FunType (t, _)) -> iden == t) predefinedFunctions2 of 
                        (Just (FunType y)) -> y 
                        _ -> throw $ MissingVar $ "function " ++ iden ++ " not defined"
            _ -> throw $ MissingVar $ "function " ++ iden ++ " not defined"

getTypeOfExpr :: Env -> TAST.Expr -> Typ 
getTypeOfExpr env (Ident var _) = case vars env DM.!? var  of 
    Just t -> t 
    Nothing -> throw $ TypeE $ show var ++ " is not instantiated"
getTypeOfExpr _ Literal {tLit=l} = case l of 
    (TLI _) -> IntType 
    (TLB _) -> BoolType
getTypeOfExpr _ (TAST.BinOp _ _ _ t) = t
getTypeOfExpr _ (TAST.FunCall _ _ t) = t
getTypeOfExpr _ (TAST.Closure  _ t) = t
getTypeOfExpr _ (TAST.Range  _ _) = IntType
getTypeOfExpr _ (TAST.ArrLookUp _ _ t) = t
getTypeOfExpr _ (TAST.Length _ t) = t
getTypeOfExpr _ (TAST.ArrLit _ t) = t

typeCheckExpr :: Env -> Ast.Expr -> (TAST.Expr, TAST.Typ)
typeCheckExpr _ LitExp {lit = LI i} = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr _ LitExp {lit = LB i} = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr env IExp {ident = str} = 
    let lup = vars env DM.!? str in 
    case lup of 
        Just t -> (Ident str t, t)
        Nothing -> throw $ MissingVar $ "variable: " ++ str ++ " has not been defined"
typeCheckExpr env Ast.BinOp {lhs=l, op=operator, rhs=r} = 
    let (leftExp, ltyp) = typeCheckExpr env l in
    let (rightExp, rtyp) = typeCheckExpr env r in
    let opType = typeOfFunction operator in
    if ltyp == rtyp then 
        (TAST.BinOp leftExp (astOpToTASTOp operator) rightExp opType, opType)
    else 
    throw $ MissingVar $ "type mismatch\nleft side: " ++ show ltyp ++ "\nright side: " ++ show rtyp 
typeCheckExpr env (Ast.FunCall fvar args) = 
    let (fname, paramTyps) = case fvar of 
            (IExp {ident = ide} ) -> lookupFunc env ide
            _ -> throw $ FunCallException $ show fvar ++ " not a valid function call"
    in
    let typedArgs = Data.List.map (\(arg, pType) -> 
                let (typeArg, typ) = typeCheckExpr env arg in
                if pType == typ  then 
                    typeArg else throw $ FunCallException $ show fvar ++ " is called with wrong types\nExpected type: " ++ show paramTyps ++ "\nActual Type: " ++ show typ 
            ) $ zip args paramTyps
    in
    let rType = last paramTyps in 
    if fname == "length" then (TAST.Length (head typedArgs) rType, rType)
    else (TAST.FunCall (Ident fname rType) typedArgs rType, rType) 

typeCheckExpr env (Ast.Range start end) = 
    let tstart = assertType env start TAST.IntType in
    let tend = assertType env end TAST.IntType in
    (TAST.Range tstart tend, TAST.IntType)

typeCheckExpr env (Ast.Closure stm) = 
    let (typedStm, _) = typeCheckStm env  stm  in
    (TAST.Closure typedStm IntType, IntType)

typeCheckExpr env (Ast.ArrLit lits) = 
    let (texps, typs) = unzip $ Prelude.foldl (\acc lit -> typeCheckExpr env lit : acc) [] lits 
    in let fall = all (\t -> t == head typs) typs
    in if fall 
        then (TAST.ArrLit texps (Pointer $ head typs), Pointer $ head typs)
        else throw $ TypeE "types in array literal are not the same"

typeCheckExpr env (Ast.ArrLookUp arr lup) = 
    let foo (Pointer t) = t 
        foo _ = throw $ TypeE "not a pointer"
    in
    let tlup = assertType env lup IntType 
    in let (lexpr, lt) = typeCheckExpr env arr 
    in (TAST.ArrLookUp lexpr tlup (foo lt), foo lt)





assertType :: Env -> Ast.Expr -> TAST.Typ -> TAST.Expr 
assertType env expr (Pointer _) = 
    let (texp, exptyp) = typeCheckExpr env expr in 
    case exptyp of 
        (Pointer _) -> texp 
        _ -> throw $ TypeE "wrong type"
assertType env expr typ = 
    let (texp, exptyp) = typeCheckExpr env expr in 
    if exptyp == typ then texp else throw $ TypeE "wrong type"




typeCheckStm :: Env -> Ast.Stm -> (TAST.Stm, Env)
typeCheckStm env (Ast.ForLoop ident iter body) = 
    let (iterExp, titer) = typeCheckExpr env iter in
    let newenv = insertIntoEnv ident titer env in -- created variable has to be a resulting type of the iterator  
    let (bexp, bodyt) = typeCheckExpr newenv body in
    (TAST.ForLoop ident iterExp bexp titer, env)
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
typeCheckStm env (Ast.IfThenElse cond th el) = 
    let (tcnd, ctyp) = typeCheckExpr env cond in 
    if ctyp /= BoolType then
        throw $ TypeE "if condition not is not a boolean"
    else 
    let (thexp, _) = typeCheckExpr env th in 
    let (elexp, _) = typeCheckExpr env el in 
    (TAST.IfThenElse tcnd thexp elexp, env)

typeCheckStm env (Ast.VarInst varIdent bd) = 
    let (texpr, exprtype) = typeCheckExpr env bd in
    let newenv = insertIntoEnv varIdent exprtype env in 
    (TAST.LetIn varIdent texpr exprtype, newenv)

typeCheckFunction :: Env ->  Ast.Function -> (TAST.Function, Env)
typeCheckFunction env func = 
    let pms = typeCheckParams (Ast.params func) in
    let newenv = Prelude.foldr (\(str, typ) acc -> insertIntoEnv str typ acc) env pms in 
    let (newbody, env') = typeCheckStm newenv $ Ast.body func in
    let tfunc = (TAST.Function 
                    {TAST.funName = Ast.funName func, TAST.params = pms
                    , TAST.body = newbody, TAST.returnType = IntType}
                ) in
    let env'' = env {functions = tfunc : functions env'} in
    (tfunc, env'')
        where 
        typeCheckParams :: [(String, String)] -> [(String, Typ)]
        typeCheckParams xs = do x <- xs 
                                case x of 
                                    (a, "Int") -> return (a, IntType)
                                    (b, "Bool") -> return (b, BoolType)
                                    (c, unknown) ->
                                        case splitStr ' ' unknown of
                                            ("ptr":ys) -> do 
                                                let cc = [(c, y) | y <- ys]
                                                (s, typ) <- typeCheckParams cc 
                                                return (s, Pointer typ)
                                            e -> throw $ TypeE $ "Type: " ++ show e ++ " is not recognised in fh"

functionHeaders :: Env -> Ast.Function -> Env 
functionHeaders env func =  
    let rtyp = helper ("", Ast.returnType func) in
    let argTyps = foldl (\acc x ->  helper x : acc) [] $ Ast.params func in
    let pms = FunType (Ast.funName func, argTyps ++ [rtyp]) in
    insertIntoEnv (Ast.funName func) pms env
    where 
        helper :: (String, String) -> Typ
        helper (_, "Int") = IntType 
        helper (_, "Bool") = BoolType 
        helper (_, x) = case splitStr ' ' x of
                            ("ptr":ys) ->  Pointer $  curry helper "" $ head ys 
                            e -> throw $ TypeE $ "Type: " ++ show e ++ " is not recognised in fh"

splitStr :: Char -> String -> [String]
splitStr b s =  helper b s [] where 
    helper :: Char -> String -> String -> [String]
    helper _ [] acc = [reverse acc]
    helper c (x:xs) acc 
        | c == x = reverse acc : helper c xs [] 
        | otherwise =  helper c xs (x : acc)

typeCheckAst :: Ast.Ast -> TypedAst
typeCheckAst funs =
    let initenv = Prelude.foldl functionHeaders emptEnv funs
    in reverse . fst $ Prelude.foldl (\(acc, env) x -> 
                                    let fenv = functionHeaders env x in 
                                    let (stm, newenv) = typeCheckFunction fenv x in
                                    (stm : acc, newenv)
                                    ) ([], initenv) funs

