module CodeGen where 
import TypedAst
import Data.List (uncons, findIndex)
import Lib (stdLib)

type Reg = Int
data Operand = Lit Int | StrLit String | R Reg | SP Int | OffSet Int | Nop | LR | AccThenUp Int | SPP
    deriving (Eq)
instance Ord Operand where 
    (<=) (SP i) (SP j) = i <= j
    (<=) (SP _) _ = False
    (<=) _ (SP _) = True
    (<=) _ _ = True

instance Show Operand where 
    show (Lit i) =  show i
    show (StrLit s) = show s 
    show (R r) = "X" ++ show r 
    show (SP i) = "[SP, " ++ "#-" ++ show i ++ "]!"
    show LR = "LR"
    show (OffSet i) = "[SP, " ++ show i ++ "]"
    show (AccThenUp i) = "[SP], " ++ show i
    show SPP = "SP"
    show Nop = ""


data Operation = Mov | Svc Int | Ldr | Add | Str | Sub | BL String | Ret | Mul

instance Show Operation where 
    show Mov = "mov"
    show (Svc i) = "svc #0x0" ++ show i
    show Ldr = "ldr"
    show Add = "add"
    show Str = "str"
    show Sub = "sub"
    show (BL s) = "bl _" ++ s
    show Mul = "mul"
    show Ret = "ret"

data CodeLine = CL Operation [Operand] 


instance Show CodeLine where
    show (CL (BL func) []) = "bl _" ++ func ++ "\n\t"
    show (CL (Svc i) []) = "svc #0x80\n"
    show (CL Ldr [R r, SP i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "\n\t"   
    show (CL Ldr [R r, OffSet i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP, #" ++ show i ++ "]\n\t"   
    show (CL Ldr [R r, AccThenUp i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "]\n\t"   
    show (CL Ret []) = "ret\n" 
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
       let a = Prelude.foldl (\acc x -> acc ++ helper x)  (header ++ "\n\n") $ blocks c  
            where 
            helper :: (String, Block) -> String 
            helper (label, block) =
                label ++ ":\n\t" ++ Prelude.foldr (\cl acc ->  acc ++ show cl) "" block
        in 
        let epilogue = 
                "\n.data \
                \\nnum: .asciz \"%d\\n\" \
                \\n.align 4\
                \\n.text"

        in
        a ++ stdLib ++ epilogue
    -- should be correct when there are multiple blocks
    -- header ++ Prelude.foldr (\(label, block) acc -> label ++ "\n\t" ++ Prelude.foldl (\acc x -> acc ++show x) "" block ++ acc) "" (blocks c)
    

addLine :: CodeLine -> BuildLet Operand
addLine  cl@(CL (BL _) []) = 
    let f (cfg, env) = (cfg {insns = cl : insns cfg}, env,R 0) in 
    BuildLet f
addLine  cl@(CL Str (_:op:_)) = 
    let f (cfg, env) = (cfg {insns = cl : insns cfg}, env, op) in 
    BuildLet f
addLine  cl@(CL _ (op:_)) = 
    let f (cfg, env) = (cfg {insns = cl : insns cfg}, env, op) in 
    BuildLet f
addLine  cl =
    let f (cfg, env) = (cfg {insns = cl : insns cfg}, env, Nop) in 
    BuildLet f

addBlock :: String -> Cfg -> Cfg 
addBlock s cfg = Cfg {blocks = (s, insns cfg): blocks cfg, insns = []}


emptyCfg :: Cfg
emptyCfg = Cfg {blocks=[], insns = []}


newtype BuildLet a = BuildLet ((Cfg , Env) -> (Cfg, Env, a))

instance Functor BuildLet where 
    fmap f (BuildLet a) =
        let r b =
                let (cfg, env, op) = a b in
                (cfg, env, f op)
        in
        BuildLet r

instance Applicative BuildLet where 
    pure a = BuildLet (\(cfg, env) -> (cfg, env, a))
    BuildLet a <*> BuildLet b =
        let f a3 =
                let (c, e, ab) = a a3 in 
                let (d, g, h) = b a3 in 
                (d, g, ab h)
        in

        BuildLet f


instance Monad BuildLet where 
    BuildLet a >>= f = 
        let bl p =
                let (cfg, env, op) = a p in 
                let BuildLet b = f op in
                let (cfg2, env2, op2) = b (cfg, env) in
                (cfg2, env2, op2)
        in 
        BuildLet bl

data Env = Env {stack :: Int, regs :: [Reg], vars :: [(String, Operand)], poppedFromStack :: [(String, Operand)]} 
    deriving (Show)
    
emptyEnv :: Env 
emptyEnv = Env {stack=0, regs= [1,2,3,4,5,6,7,8,9,10,11,12,13,14, 0], vars = [], poppedFromStack = []}




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


insertIntoEnv :: String -> Operand -> BuildLet Operand 
insertIntoEnv s op = 
    let f (cfg, env)  =
            case op of 
                SP _ ->  
                    let maxs = stack env in 
                    (cfg, env {vars = (s , SP (maxs + 16)) : vars env}, op)
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




popFromStack2 :: String -> [(String, Operand)] -> Env -> BuildLet Operand
popFromStack2 str variables env = 
    let index = findIndex (\(var, _) -> var == str) variables in 
    case index of 
        Just i -> do 
            popped <- popReg  
            addLine $ CL Ldr [popped, OffSet (i*16)] 
        Nothing -> error $ "The variable: " ++ str ++ " is not on the stack"



-- codegenExpr: constructs a CFG from a given expression
-- Params: Env ~ the environment  
--       : Expr ~ The expression to generate code for. 
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenExpr :: Expr -> BuildLet Operand
codegenExpr (Ident str) = 
    BuildLet (\bl -> 
        let env = getEnv2 bl in
        case Prelude.lookup str $ vars env of 
            Just (R i) -> 
                (fst bl, snd bl, R i)
            Just _ -> 
                let BuildLet a = popFromStack2 str (vars env) env in 
                a bl
            Nothing -> error $ "variable " ++ str ++ " not in environment"
            )
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
    in
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
            (Ident str) -> str 
            _ -> error "malformed function call"
    _ <- fst $ Prelude.foldr (\arg (acc, i) -> 
                let code = do
                        op <- codegenExpr arg
                        addLine $ CL Mov [R i, op] 
                in (code, i + 1)
            ) (pure (R 0), 0) args
    
    addLine  $ CL (BL fn) []

-- codegenStm: constructs a CFG from a given statement
-- Params: Env ~ the environment  
--       : Stm ~ The statement to generate code for. It is also a node in the CFG
-- Return: BuildLet ~ State monad, in which the state is a CFG
codegenStm :: Stm -> BuildLet Operand
codegenStm  (LetIn ident expr _) =  do
    a <- codegenExpr expr
    b <- addLine $ CL Str [a, SP 16]
    insertIntoEnv ident b 

codegenStm (Return expr _) = do
            a <- codegenExpr expr 
            case a of 
                    (SP i) -> addLine $ CL Ldr [R 0, SP i]
                    _ -> addLine  $ CL Mov [R 0, a] 

codegenStm (Scope stms) = 
    Prelude.foldl (\acc stm ->  
                        let a = codegenStm stm 
                        in
                        acc >> a
                ) (return (R 0)) stms 

codegenStm _ = error  "not implemented yet for stm"

getEnv :: BuildLet Operand -> Env 
getEnv (BuildLet a) = let (_, e,_) = a (emptyCfg, emptyEnv) in e

funEpilogue :: BuildLet Operand
funEpilogue = 
    BuildLet (\(cfg, env) -> 
        let a = addLine $ CL Ldr [LR, AccThenUp (16 * (1 + length (getStackVars env)))] in
        let BuildLet b = a >> addLine (CL Ret [] ) in
        b (cfg, env)
        )

codegenFunc :: Function -> BuildLet Operand
codegenFunc func = do 
    let label = if funName func /= "main" then "_" ++ funName func else "_start"
    _ <- if funName func == "main" then
                return Nop
            else 
                addLine $ CL Str [LR, SP 16]

    _ <- fst . Prelude.foldl (\(acc, iter) (str, _) -> ( 
                                                let e = insertIntoEnv str (R iter) 
                                                in acc >> e >> popSpecReg iter , iter + 1)
                                        ) (return Nop, 0) $ params func

    _ <- codegenStm $ body func 
    _ <- if funName func == "main" then do 
            _ <- addLine $ CL Mov [R 16, Lit 1]
            addLine  (CL (Svc 0) [] )
        else funEpilogue
    BuildLet (\(c, e) -> (addBlock label c, e, R 0))

codegenAst :: TypedAst -> String 
codegenAst funcs = 
    let BuildLet cfg = Prelude.foldl (\acc stm -> 
                let buildlet = codegenFunc stm in
                acc >> buildlet
            ) (return Nop) funcs in
    let (cfg', _, _) = cfg (emptyCfg, emptyEnv) in
    printCfg cfg'
    

header :: String 
header = ".global _start\n.align 4\n"


generatePrintFunc :: BuildLet Operand
generatePrintFunc = do
    _ <- addLine (CL Str [LR, SP 16])
    _ <- addLine (CL Mov [R 1, R 0])
    _ <- addLine $ CL Str [R 1, SP 16]
    _ <- addLine $ CL (BL "_printf") []
    _ <- addLine $ CL Add [SPP, SPP, Lit 32]
    _ <- addLine (CL (Svc 0) [])
    _ <- addLine $ CL Ldr [LR, AccThenUp 16]
    _ <- addLine (CL Ret [] )
    BuildLet (\(c, e) -> (addBlock "_print" c, e, R 0))
    




