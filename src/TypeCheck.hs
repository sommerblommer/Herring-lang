module TypeCheck where 
import Ast 
import TypedAst as TAST
import qualified Data.Map as DM
import Data.List 
import Control.Exception (throw)
import Exceptions 
import Control.Monad.State (State, MonadState (get, put), modify, runState)

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

data Env = Env {vars :: DM.Map String Typ, functions :: [TAST.Function], types :: [TAST.TypeDecl]}
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
typeCheckExpr _ (LitExp (LI i) _) = (TAST.Literal {tLit = TLI i}, IntType)
typeCheckExpr _ (LitExp (LB i) _) = (TAST.Literal {tLit = TLB i}, IntType)
typeCheckExpr env (IExp str loc) = 
    let lup = vars env DM.!? str in 
    case lup of 
        Just t -> (Ident str t, t)
        Nothing -> throw $ MissingVar $ "variable: " ++ str ++ ", at: " ++ show loc ++ " has not been defined"
typeCheckExpr env (Ast.BinOp l operator r loc) = 
    let (leftExp, ltyp) = typeCheckExpr env l in
    let (rightExp, rtyp) = typeCheckExpr env r in
    let opType = typeOfFunction operator in
    if ltyp == rtyp then 
        (TAST.BinOp leftExp (astOpToTASTOp operator) rightExp opType, opType)
    else 
    throw $ MissingVar $ "type mismatch at: " ++ show loc ++ "\nleft side: " ++ show ltyp ++ "\nright side: " ++ show rtyp 
typeCheckExpr env (Ast.FunCall fvar args floc) = 
    let (fname, paramTyps) = case fvar of 
            (IExp ide _ ) -> lookupFunc env ide
            _             -> throw $ FunCallException $ show fvar ++ " not a valid function call at: " ++ show floc
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

typeCheckExpr env (Ast.Range start end _) = 
    let tstart = assertType env start TAST.IntType in
    let tend = assertType env end TAST.IntType in
    (TAST.Range tstart tend, TAST.IntType)

typeCheckExpr env (Ast.Closure stm _) = 
    let (typedStm, _) = runState (typeCheckStm stm) env  in
    (TAST.Closure typedStm IntType, IntType)

typeCheckExpr env (Ast.ArrLit lits loc) = 
    let (texps, typs) = unzip $ Prelude.foldl (\acc lit -> typeCheckExpr env lit : acc) [] lits 
    in let fall = all (\t -> t == head typs) typs
    in if fall 
        then (TAST.ArrLit texps (Pointer $ head typs), Pointer $ head typs)
        else throw $ TypeE $ "types in array literal are not the same at: " ++ show loc

typeCheckExpr env (Ast.ArrLookUp arr lup loc) = 
    let foo (Pointer t) = t 
        foo _ = throw $ TypeE $ "lookup at: " ++ show loc ++ " is not at pointer"
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
    if exptyp == typ 
        then texp 
        else throw $ TypeE $ "wrong type\nExpected: " 
                             ++ show typ 
                             ++ "\nActual: " 
                             ++ show exptyp



typeCheckStm :: Ast.Stm -> State Env TAST.Stm
typeCheckStm (Ast.ForLoop ident iter body _) = do
    env <- get 
    let (iterExp, titer) = typeCheckExpr env iter 

    -- created variable has to be a resulting type of the iterator, also is out of scope after loop
    let newenv = insertIntoEnv ident titer env  
    let (bexp, _) = typeCheckExpr newenv body 
    return $ TAST.ForLoop ident iterExp bexp titer

typeCheckStm (Ast.LetIn str expr _) = do
    env <- get 
    let (texpr, exprtype) = typeCheckExpr env expr 
    modify (insertIntoEnv str exprtype) 
    return $ TAST.LetIn str texpr exprtype

typeCheckStm (Ast.Scope stms) = TAST.Scope <$> traverse typeCheckStm stms

typeCheckStm  (Ast.Return expr _) = do
    env <- get
    let (texpr, exprtype) = typeCheckExpr env expr 
    return $ TAST.Return texpr exprtype
typeCheckStm  (Ast.Exp expr _) = do 
    env <- get 
    let (texpr, exprtype) = typeCheckExpr env expr
    return $ TAST.StmExpr texpr exprtype
typeCheckStm  (Ast.IfThenElse cond th el _) = do 
    env <- get
    let tcnd = assertType env cond BoolType 
    let (thexp, _) = typeCheckExpr env th  
    let (elexp, _) = typeCheckExpr env el  
    return $ TAST.IfThenElse tcnd thexp elexp

typeCheckStm  (Ast.VarInst varIdent bd _) = do 
    env <- get
    let (texpr, exprtype) = typeCheckExpr env bd
    modify $ insertIntoEnv varIdent exprtype 
    return $ TAST.LetIn varIdent texpr exprtype

typeCheckFunction :: Env ->  Ast.Function -> (TAST.Function, Env)
typeCheckFunction env func = 
    let pms = typeCheckParams (Ast.params func) in
    let newenv = Prelude.foldr (\(str, typ) acc -> insertIntoEnv str typ acc) env pms in 
    let (newbody, env') = runState (typeCheckStm $ Ast.body func) newenv  in
    let tfunc = (TAST.Function 
                    {TAST.funName = Ast.funName func, TAST.params = pms
                    , TAST.body = newbody, TAST.returnType = IntType}
                ) in
    let env'' = env {functions = tfunc : functions env'} in
    (tfunc, env'')

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

typeDecls :: Env -> [Ast.TypeDecl] -> Env 
typeDecls = foldr (\td acc -> 
                let inner = typeCheckParams $ typs td
                    tt = TType {TAST.tname= Ast.tname td, innerTs=inner}
                in acc {types = tt : types acc}
            ) 

splitStr :: Char -> String -> [String]
splitStr b s =  helper b s [] where 
    helper :: Char -> String -> String -> [String]
    helper _ [] acc = [reverse acc]
    helper c (x:xs) acc 
        | c == x = reverse acc : helper c xs [] 
        | otherwise =  helper c xs (x : acc)



typeCheckAst :: Ast.Ast -> TypedAst
typeCheckAst ast =
    let tenv               = typeDecls emptEnv $ Ast.tdecls ast
        initenv            = Prelude.foldl functionHeaders tenv $ Ast.fdecls ast
        (funs, lastEnv)    = Prelude.foldl (\(acc, env) x -> 
                                    let fenv = functionHeaders env x in 
                                    let (stm, newenv) = typeCheckFunction fenv x in
                                    (stm : acc, newenv)
                                    ) ([], initenv) $ Ast.fdecls ast
    in TAST {TAST.tdecls=types lastEnv, TAST.fdecls=reverse funs}

