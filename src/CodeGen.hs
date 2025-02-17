module CodeGen where 
import TypedAst
import Data.List (uncons, findIndex)

type Reg = Int
data Operand = Lit Int | StrLit String | R Reg | SP Int | OffSet Int | Nop | LR | AccThenUp Int 
    deriving (Eq)
instance Ord Operand where 
    (<=) (SP i) (SP j) = i <= j
    (<=) (SP _) _ = False
    (<=) _ (SP _) = True
    (<=) _ _ = True

instance Show Operand where 
    show (Lit i) = "#" ++ show i
    show (StrLit s) = show s 
    show (R r) = "X" ++ show r 
    show (SP i) = "[SP, " ++ "#-" ++ show i ++ "]!"
    show LR = "LR"
    show (OffSet i) = "[SP, " ++ show i ++ "]"
    show (AccThenUp i) = "[SP], " ++ show i
    show Nop = ""

data Operation = Mov | Svc Int | Ldr | Add | Str | Sub | BL String | Ret

instance Show Operation where 
    show Mov = "mov"
    show (Svc i) = "svc " ++ show i
    show Ldr = "ldr"
    show Add = "add"
    show Str = "str"
    show Sub = "sub"
    show (BL s) = "bl _" ++ s
    show Ret = "ret"

data CodeLine = CL Operation [Operand] 


instance Show CodeLine where
    show (CL (BL func) []) = "bl _" ++ func ++ "\n\t"
    show (CL (Svc i) []) = "svc " ++ show i ++ "\n"
    show (CL Ldr [R r, SP i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "\n\t"   
    show (CL Ldr [R r, OffSet i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP, #" ++ show i ++ "]\n\t"   
    show (CL Ldr [R r, AccThenUp i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "]\n\t"   
    show (CL op opl) = show op ++ " " ++ printOperandList opl ++ "\n\t"

printOperandList :: [Operand] -> String 
printOperandList [] = ""
printOperandList [o] = show o 
printOperandList (op:ops) = show op ++ ", " ++ printOperandList ops


type Block = [CodeLine]



data Cfg = Cfg {blocks::[(String, Block)], insns :: [CodeLine]}
    deriving (Show)

printCfg :: Cfg -> String 
printCfg c =
      Prelude.foldl (\acc x -> acc ++ helper x)  (header ++ "\n\n") $ blocks c  
        where 
        helper :: (String, Block) -> String 
        helper (label, block) =
             label ++ ":\n\t" ++ Prelude.foldr (\cl acc ->  acc ++ show cl) "" block
    -- should be correct when there are multiple blocks
    -- header ++ Prelude.foldr (\(label, block) acc -> label ++ "\n\t" ++ Prelude.foldl (\acc x -> acc ++show x) "" block ++ acc) "" (blocks c)
    

addLine :: Env -> CodeLine -> BuildLet (Operand, Env)
addLine e cl@(CL (BL _) []) = BuildLet (\cfg -> cfg {insns = cl : insns cfg}, (R 0, e))
addLine e cl@(CL Str (_:op:_)) = BuildLet (\cfg -> cfg {insns = cl : insns cfg}, (op, e))
addLine e cl@(CL _ (op:_)) = BuildLet (\cfg -> cfg {insns = cl : insns cfg}, (op, e))
addLine e cl =BuildLet (\cfg -> cfg {insns = cl : insns cfg}, (Nop, e))

addBlock :: String -> Cfg -> Cfg 
addBlock s cfg = Cfg {blocks = (s, insns cfg): blocks cfg, insns = []}


emptyCfg :: Cfg
emptyCfg = Cfg {blocks=[], insns = []}


newtype BuildLet a = BuildLet (Cfg -> Cfg, a)

instance Functor BuildLet where 
    fmap f (BuildLet (c, a)) = 
        BuildLet (c, f a)

instance Applicative BuildLet where 
    pure a = BuildLet (id, a)
    BuildLet (f, a) <*> BuildLet (c, b) = 
        BuildLet (f . c, a b)


instance Monad BuildLet where 
    BuildLet (c, a) >>= f = 
        let BuildLet (c', b) = f a in 
        let combined = c' . c in 
        BuildLet (combined, b)

data Env = Env {stack :: Int, regs :: [Reg], vars :: [(String, Operand)], poppedFromStack :: [(String, Operand)]} 
    deriving (Show)
    
emptyEnv :: Env 
emptyEnv = Env {stack=0, regs= [1,2,3,4,5,6,7,8,9,10,11,12,13,14, 0], vars = [], poppedFromStack = []}



-- >>> updateStack (R 1) emptyEnv
-- Env {stack = 0, regs = [0,2,3,4,5,6,7,8,9,10,11,12,13,14], vars = [], poppedFromStack = []}

updateStack :: Operand -> Env -> Env
updateStack (R r) e = e {regs = if r `elem` regs e then Prelude.filter (/= r) (regs e) else r : regs e}
updateStack (SP _) e = e {stack = 16 + stack e}
updateStack _ e = e

ups :: BuildLet (Operand, Env) -> Env 
ups (BuildLet (_, (op, env))) = updateStack op env

popSpecReg :: Env -> Int -> Env 
popSpecReg env i = env {regs = filter (/= i) $ regs env}  

-- will pop a free register if there is one or will return an updated stack position
popReg :: Env -> (Env, Operand)
popReg e = let a = uncons $ regs e in 
    case a of 
        Just (r, t) -> (e {regs = t}, R r)
        Nothing -> (updateStack (SP 16) e, SP $ 16 + stack e) 

insertIntoEnv :: String -> Operand -> Env -> Env 
insertIntoEnv s op env = case op of 
        SP _ ->  
            let maxs = stack env in 
            env {vars = (s , SP (maxs + 16)) : vars env}
        _ ->  env {vars = (s, op) : vars env}

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
getOp :: BuildLet (Operand, Env) -> Operand 
getOp (BuildLet (_, (op, _))) = op



-- Ugly function that works should be refactored. Found out i did wrong too
popFromStack :: String -> [(String, Operand)] -> Env -> (BuildLet (Operand, Env), Operand, [String])
popFromStack str ((cvar, cop):rest) env
    | str /= cvar = 
        let (env', nop) = popReg env in
        let l = addLine env' (CL Ldr [nop,  SP 16]) in 
        let (b, lasterEnv, sl) = popFromStack str rest (getEnv l)  in
        let push = addLine (getEnv b) (CL Str [getOp l,  SP 16]) in 
        (l >> b >> push, lasterEnv, cvar : sl)
    | otherwise = 
        let (env', nop) = popReg env in
        let a =  addLine env' (CL Ldr [nop,  SP 16]) in
        let push = addLine (getEnv a) (CL Str [getOp a,  SP 16]) in 
        (a >> push, getOp a, [])
popFromStack _ _ env = (return (Nop, env), Nop, [])


popFromStack2 :: String -> [(String, Operand)] -> Env -> BuildLet (Operand, Env)
popFromStack2 str variables env = 
    let index = findIndex (\(var, _) -> var == str) variables in 
    case index of 
        Just i -> 
            let (nenv, popped) = popReg env in
            addLine nenv $ CL Ldr [popped, OffSet (i*16)] 
        Nothing -> error $ "The variable: " ++ str ++ " is not on the stack"

-- OBSOLETE
codegenAndHandleOp :: Env -> Expr -> BuildLet (Operand, Env)
codegenAndHandleOp e = handleOperand e .  codegenExpr e


-- codegenExpr: constructs a CFG from a given expression
-- Params: Env ~ the environment  
--       : Expr ~ The expression to generate code for. 
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenExpr :: Env -> Expr -> BuildLet (Operand, Env)
codegenExpr env (Ident str) = case Prelude.lookup str $ vars env of 
    Just (R i) -> 
        BuildLet (id, (R i, env))
    Just _ -> do
        popFromStack2 str (vars env) env 
    Nothing -> error $ "variable " ++ str ++ " not in environment"
codegenExpr env (Literal {tLit=TLI i}) = 
    let (env', op) = popReg env in
    addLine env' $ CL Mov [op, Lit i]
codegenExpr env (Literal {tLit=TLB b}) 
    | b =  return (Lit 1, env)
    | otherwise = return (Lit 0, env)
codegenExpr env (BinOp l op r _) = 
    let binop = case op of 
            Plus -> Add 
            Minus -> Sub
    in
    case l of 
        (BinOp {}) ->  do
            lused <- codegenAndHandleOp env l
            rused <- codegenAndHandleOp (snd lused) r 
            let (le, nextop) = popReg $ snd rused
            addLine le $ CL binop [nextop, fst lused, fst rused]
        _ -> do 
            rused <- codegenAndHandleOp env r 
            lused <- codegenAndHandleOp (snd rused) l
            let (le, nextop) = popReg $ snd lused
            addLine le $ CL binop [nextop, fst lused, fst rused]
codegenExpr env (FunCall fname args _) = do
    let fn = case fname of    
            (Ident str) -> str 
            _ -> error "malformed function call"
    _ <- fst $ Prelude.foldr (\arg (acc, i) -> 
                let code = do
                        (op, nenv) <- codegenExpr (getEnv acc) arg
                        addLine nenv $ CL Mov [R i, op] 
                in (code, i + 1)
            ) (BuildLet (id, (R 0, env)), 0) args
    
    addLine env $ CL (BL fn) []

-- codegenStm: constructs a CFG from a given statement
-- Params: Env ~ the environment  
--       : Stm ~ The statement to generate code for. It is also a node in the CFG
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenStm :: Env ->  Stm -> BuildLet (Operand, Env)
codegenStm env (LetIn ident expr _) =  
    let BuildLet (cfg, b) = do
            (a, e) <- codegenAndHandleOp env expr
            addLine e $ CL Str [a, SP 16]
    in
    let newenv = insertIntoEnv ident (fst b) env in 
    BuildLet (cfg, (fst b, newenv))
codegenStm env (Return expr _) = do
            a <- codegenAndHandleOp env expr 
            case a of 
                    (SP i, e) -> addLine e $ CL Ldr [R 0, SP i]
                    (_, e) -> addLine e  $ CL Mov [R 0, fst a] 
codegenStm env (Scope stms) = 
    Prelude.foldl (\acc stm ->  
                                let a = codegenStm (getEnv acc) stm 
                                in
                                acc >> a
                ) (BuildLet (id, (R 0, env))) stms 
codegenStm _ _ = error  "not implemented yet for stm"

getEnv :: BuildLet (Operand, Env) -> Env 
getEnv (BuildLet (_ ,(_, e))) = e

codegenFunc :: Env -> Function -> (BuildLet Operand, Env)
codegenFunc env func = do 
    let label = "_" ++ funName func 
    let prelude = if funName func == "main" then
            BuildLet (id, (Nop, env)) 
            else 
                addLine env $ CL Str [LR, SP 16]

    let newnev = fst . Prelude.foldl (\(acc, iter) (str, _) -> (let e = insertIntoEnv str (R iter) acc in popSpecReg e iter , iter + 1)) (env, 0) $ params func
    let buildlet = codegenStm newnev $ body func 
    let ending = if funName func == "main" then do 
            _ <- addLine env $ CL Mov [R 16, Lit 1]
            addLine env (CL (Svc 0) [] )
        else do    
            _ <- addLine newnev $ CL Ldr [LR, AccThenUp (16 * (1 + length (getStackVars newnev)))]
            addLine env (CL Ret [] )
            
    (prelude >> buildlet >> ending >> BuildLet (addBlock label, R 0), getEnv buildlet)

codegenAst :: TypedAst -> String 
codegenAst funcs = 
    let BuildLet (cfg, _) = fst $ Prelude.foldl (\(acc, env) stm -> 
                let (buildlet, _) = codegenFunc env stm in
                (acc >> buildlet, env)
            ) (BuildLet (id, R 0), emptyEnv) funcs in
    printCfg $ cfg emptyCfg
    

header :: String 
header = ".global _main\n.align 2\n"






