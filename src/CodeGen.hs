module CodeGen where 
import TypedAst
import Data.List (uncons, findIndex)
import BuildLet  

   -- should be correct when there are multiple blocks
    -- header ++ Prelude.foldr (\(label, block) acc -> label ++ "\n\t" ++ Prelude.foldl (\acc x -> acc ++show x) "" block ++ acc) "" (blocks c)
    



getUid :: BuildLet Operand 
getUid  = 
    BuildLet ( \(cfg, env) -> 
        let nenv = env {uid = uid env + 1} in
        (cfg, nenv, Lit (uid env))
    )



updateStack :: Operand -> Env -> Env
updateStack (R r) e = e {regs = if r `elem` regs e then Prelude.filter (/= r) (regs e) else r : regs e}
updateStack (SP _) e = e {stack = 16 + stack e}
updateStack _ e = e


popSpecReg :: Int -> BuildLet Operand 
popSpecReg i =
    let f (cfg, env) = 
            (cfg, env {regs = filter (/= i) $ regs env}, Nop)
    in BuildLet f


-- will pop a free register if there is one or will return an updated stack position
popReg :: BuildLet Operand
popReg = 
    let f (cfg, env) = 
            let a = uncons $ regs env in
            case a of 
                Just (r, t) ->  (cfg, env {regs = t}, R r)
                Nothing -> (cfg,  updateStack (SP 16) env, SP $ 16 + stack env) 
    in 
    BuildLet f

addDeferedStore :: String -> Operand -> BuildLet Operand 
addDeferedStore s (Defer op) = 
    BuildLet (\(cfg, env) -> 
        (cfg, env {vars = (s , Defer op) : vars env, deferedStores = (s, Defer op) : deferedStores env}, Defer op)
    )
addDeferedStore _ _ = idBuildlet

insertIntoEnv :: String -> Operand -> BuildLet Operand 
insertIntoEnv s op = 
    let f (cfg, env)  =
            case op of 
                SP _ ->  
                    let maxs = stack env in 
                    (cfg, env {stack = stack env + 1, vars = (s , SP (maxs + 16)) : vars env}, op)
                _ ->  (cfg, env {vars = (s, op) : vars env}, op)
    in BuildLet f

findMaxOnStack :: Env -> Int 
findMaxOnStack e = 
    Prelude.foldr (\(_, x) acc ->
            case x of 
                SP i -> max i acc
                _ -> acc
    ) 0 $ vars e

-- Creates a new buildlet if operand is an item on the stack 
-- TODO create a new function that pops variables from the stack 
-- if there are any items on top of it
handleOperand :: Env -> BuildLet (Operand, Env) -> BuildLet (Operand, Env)
handleOperand _ b = b

getStackVars :: Env -> [(String, Operand)]
getStackVars = helper . vars  where 
    helper :: [(String, Operand)] -> [(String, Operand)]
    helper ((s, SP i):rest) = (s, SP i): helper rest 
    helper (_:rest) = helper rest
    helper [] = []

getOp :: BuildLet Operand -> Operand 
getOp (BuildLet a) = let (_,_, op) = a (emptyCfg, emptyEnv) in op

getEnv2 :: (Cfg, Env) -> Env 
getEnv2 (_, env)= env



idBuildlet :: BuildLet Operand 
idBuildlet = BuildLet (\(cfg, env) -> (cfg, env, Nop))  


storeToVar :: Operand -> String -> BuildLet Operand 
storeToVar op ident = do 
    env <- getEnv idBuildlet 
    case Prelude.lookup ident $ vars env of 
        Just (R _) -> error "variable not on stack"
        Just _ -> 
                let index = findIndex (\(var, _) -> var == ident) $ vars env in
                case index of 
                    Just i -> addLine $ MoffSet Str [op, SPP, Lit i]
                    _ -> error "variable not found"
        _ -> addLine $ MoffSet Str [op, SPP, Defer ident]

popVar :: String -> BuildLet Operand
popVar str = BuildLet (\bl -> 
    let env = getEnv2 bl in
    case Prelude.lookup str $ vars env of 
        Just (R i) -> 
            (fst bl, snd bl, R i)
        Just _ -> 
            let BuildLet a = popFromStack2 str (vars env) in 
            a bl
        Nothing -> error $ "variable " ++ str ++ " not in environment"
    ) where 
    popFromStack2 :: String -> [(String, Operand)] -> BuildLet Operand
    popFromStack2 str2 variables = 
        let index = findIndex (\(var, _) -> var == str2) variables in 
        case index of 
            Just i -> do 
                popped <- popReg  
                addLine $ MoffSet Ldr [popped, SPP, Defer str2] 
            Nothing -> error $ "The variable: " ++ str2 ++ " is not on the stack"



-- codegenExpr: constructs a CFG from a given expression
-- Params: Env ~ the environment  
--       : Expr ~ The expression to generate code for. 
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenExpr :: Expr -> BuildLet Operand
codegenExpr (Ident str _) = popVar str
codegenExpr (Literal {tLit=TLI i}) = do
    op <- popReg  
    addLine $ CL Mov [op, Lit i]
codegenExpr (Literal {tLit=TLB b}) 
    | b =  return (Lit 1)
    | otherwise = return (Lit 0)
codegenExpr (BinOp l op r _) = 
    let binop = case op of 
            Plus -> Add 
            Minus -> Sub
            Mult -> Mul
            Lt -> Cmp
            Gt -> Cmp
            Lte -> Cmp
            Gte -> Cmp
            Eq -> Tst
    in
    if binop == Cmp || binop == Tst  then do
        lused <- codegenExpr l
        rused <- codegenExpr r 
        case op of 
            Lt -> do
                lat <- addLine $ CL Add [lused, lused, Lit 1]
                addLine $ CL binop [rused, lat]
            Lte -> do 
                addLine $ CL binop [rused, lused]
            Gt -> do
                lat <- addLine $ CL Add [rused, rused, Lit 1]
                addLine $ CL binop [lused, lat]
            Gte -> do 
                addLine $ CL binop [lused, rused]
            Eq -> do 
                addLine $ CL binop [lused, rused]
            a -> error $ "hah " ++ show a
    else 
    case l of 
        (BinOp {}) ->  do
            lused <- codegenExpr l
            rused <- codegenExpr r 
            nextop <- popReg 
            addLine $ CL binop [nextop, lused, rused]
        _ -> do 
            rused <- codegenExpr r 
            lused <- codegenExpr l
            nextop <- popReg 
            addLine $ CL binop [nextop, lused, rused]
codegenExpr (FunCall fname args _) = do
    let fn = case fname of    
            (Ident str _) -> str 
            _ -> error "malformed function call"
    _ <- fst $ Prelude.foldr (\arg (_, i) -> 
                let code = do
                        op <- codegenExpr arg
                        addLine $ CL Mov [R i, op] 
                in (code, i + 1)
            ) (pure (R 0), 0) args
    
    addLine  $ CL (BL fn) []

codegenExpr (Range _ _) = error "range not implemented"

codegenExpr (Closure stm _) = codegenStm stm 


-- codegenStm: constructs a CFG from a given statement
-- Params: Env ~ the environment  
--       : Stm ~ The statement to generate code for. It is also a node in the CFG
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenStm :: Stm -> BuildLet Operand
codegenStm (StmExpr expr _) = do 
    codegenExpr expr
codegenStm  (LetIn ident expr _) =  do
    a <- codegenExpr expr
    if ident /= "_" then do
        _ <- addLine $ MoffSet Str [a, SPP, Defer ident]
        addDeferedStore ident $ Defer ident 
        else idBuildlet

codegenStm (Return expr _) = do
            a <- codegenExpr expr 
            case a of 
                    (SP i) -> addLine $ MoffSet Ldr [R 0, SP i]
                    _ -> addLine  $ CL Mov [R 0, a] 

codegenStm (Scope stms) = 
    Prelude.foldl (\acc stm ->  
                        let a = codegenStm stm 
                        in
                        acc >> a
                ) (return (R 0)) stms 


codegenStm (IfThenElse cond th el) = do
    r <- getUid 
    env <- getEnv idBuildlet
    let ifi = case r of 
                (Lit i) -> i 
                _ -> error "not possible"
    let ifLabel = "_if" ++ show ifi
    let thenLabel = "_then" ++ show ifi
    let elseLabel = "_else" ++ show ifi
    let endLabel = "_end" ++ show ifi
    _ <- endBlock
    _ <- startBlock ifLabel
    _ <- codegenExpr cond
    _ <- addLine $ CL (B (Just BuildLet.LT)) [StrLit elseLabel]
    _ <- endBlock
    _ <- startBlock thenLabel 
    _ <- codegenExpr th 
    _ <- addLine $ CL (B Nothing) [StrLit endLabel]
    _ <- endBlock
    _ <- startBlock elseLabel
    _ <- overWriteEnv env
    _ <- codegenExpr el
    _ <- addLine $ CL (B Nothing) [StrLit endLabel]
    _ <- endBlock
    _ <- overWriteEnv env
    startBlock endLabel

codegenStm (ForLoop ident iter body _typ) = do
    env <- getEnv idBuildlet  -- save the environment before the loop 
    r <- getUid 
    let ifi = case r of 
                (Lit i) -> i 
                _ -> error "not possible"
    let condLabel = "_cond" ++ show ifi
    let endLabel = "_end" ++ show ifi
    _ <- case iter of 
                (Range start end) -> do 
                        stcode <- codegenExpr start 
                        fr <- addLine $ MoffSet Str [stcode, SPP, Lit 16]
                        _ <- insertIntoEnv ident fr
                        _ <- addLine $ CL Add [R 12, R 12, Lit 1]
                        _ <- endBlock
                        _ <- startBlock condLabel
                        a <- popVar ident
                        endcode <- codegenExpr end 
                        _ <- addLine $ CL Cmp [a, endcode]
                        addLine $ CL (B (Just BuildLet.GE)) [StrLit endLabel]
                _ -> error "should not be possible"

    _ <- codegenExpr body 
    a <- popVar ident
    added <- addLine $ CL Add [a, a, Lit 1]
    _ <- storeOnStack ident added
    _ <- addLine $ CL (B Nothing) [StrLit condLabel]
    _ <- endBlock
    _ <- startBlock endLabel 
    overWriteEnv env -- overwrite the env from the loop, as to exit the scope 


storeOnStack :: String -> Operand -> BuildLet Operand
storeOnStack ident op = do
        env <- getEnv idBuildlet
        str <- addLine $ MoffSet Str [op, SPP, Lit (16 * stack env)] 
        insertIntoEnv ident str

    



funEpilogue :: BuildLet Operand
funEpilogue = 
    BuildLet (\(cfg, env) -> 
        let a = addLine $ MopThenMove Ldr [LR, SPP, Lit $ 16 * (length (deferedStores env) + stack env)] in
        let c = addLine $ MopThenMove Ldr [LR, SPP, Lit 16] in
        let BuildLet b = a >> c >> addLine (CL Ret [] ) in
        b (cfg, env)
    )

codegenFunc :: Function -> BuildLet Operand
codegenFunc func = do 
    let label = if funName func /= "main" then "_" ++ funName func else "_start"
    _ <- overWriteEnv emptyEnv
    _ <- startBlock label 
    _ <- if funName func == "main" then
                return Nop
            else 
                addLine $ MovePThenMop Str [LR, SPP, Lit 16] -- Prepend the store operation

    _ <- fst . Prelude.foldl (\(acc, iter) (str, _) -> ( do
                                                _ <- acc
                                                a <- addLine $ MovePThenMop Str [R iter, SPP, Lit 16]
                                                _ <-  insertIntoEnv str a
                                                popSpecReg iter , iter + 1)
                                        ) (return Nop, 0) $ params func


    _ <- codegenStm $ body func 
    bodyEnv <- getEnv idBuildlet
    -- Make make space on the stack for each element to be stored later
    _ <- handleDefered bodyEnv 
    _ <- if funName func == "main" then do 
            _ <- addLine $ CL Mov [R 16, Lit 1]
            addLine  (CL (Svc 0) [] )
        else funEpilogue
    endBlock

handleDefered :: Env -> BuildLet Operand 
handleDefered env = 
    let BuildLet def =  Prelude.foldl (\acc _ -> ( do
                                    _ <- acc
                                    addLine $ MovePThenMop Str [RZR, SPP, Lit 16]
                                )) (return Nop) $ deferedStores env
    in
    let (c, _, _ ) = def (emptyCfg, emptyEnv) in
    let stores = insns c in
    BuildLet (\(cfg, benv) -> 
        let (s, b) = last $ blocks cfg in
        let newb = (s, init stores ++ b) in
        let newCfg = cfg {blocks = init (blocks cfg) ++ [newb]} in
        (newCfg, benv, Nop)
    )


codegenAst :: TypedAst -> String 
codegenAst fs = 
    let BuildLet cfg = Prelude.foldl (\acc stm -> 
                let buildlet = codegenFunc stm in
                acc >> buildlet
            ) (return Nop) fs in
    let (cfg', lenv, _) = cfg (emptyCfg, emptyEnv) in
    printCfg $ fixCfg lenv cfg'
    



generatePrintFunc :: BuildLet Operand
generatePrintFunc = do
    _ <- endBlock
    _ <- startBlock "_print" 
    _ <- addLine (MovePThenMop Str [LR, SPP, Lit 16])
    _ <- addLine (CL Mov [R 1, R 0])
    _ <- addLine $ MovePThenMop Str [R 1, SPP, Lit 16]
    _ <- addLine $ CL (BL "_printf") []
    _ <- addLine $ CL Add [SPP, SPP, Lit 32]
    _ <- addLine (CL (Svc 0) [])
    _ <- addLine $ MopThenMove Ldr [LR, AccThenUp 16]
    _ <- addLine (CL Ret [] )
    endBlock
    




