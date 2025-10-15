module BuildLet where 
import Lib (stdLib)
import Data.List (findIndex)
import Control.Monad.State 

header :: String 
header = ".global _start\n.align 4\n"
type Reg = Int

data Cfg = Cfg {blocks::[(String, Block)], insns :: [CodeLine], currentBlock :: String}
    deriving (Show)

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

data Env = Env {stack :: Int, regs :: [Reg], vars :: [(String, Operand)], deferedStores :: [(String, Operand)], poppedFromStack :: [(String, Operand)], uid :: Int} 
    deriving (Show)

data Operand = Lit Int | StrLit String | R Reg | RZR | SP Int | Nop | LR | AccThenUp Int | SPP | Defer String
    deriving (Eq)


instance Ord Operand where 
    (<=) (SP i) (SP j) = i <= j
    (<=) (SP _) _ = False
    (<=) _ (SP _) = True
    (<=) _ _ = True

instance Show Operand where 
    show (Lit i) =  show i
    show (StrLit s) = s 
    show (R r) = "X" ++ show r 
    show (SP i) = "[SP, " ++ "#-" ++ show i ++ "]!"
    show LR = "LR"
    show (AccThenUp i) = "[SP], " ++ show i
    show SPP = "SP"
    show Nop = ""
    show (Defer _) = ""
    show RZR = "XZR"

data MemOp = Ldr | Str

instance Show MemOp where 
    show Ldr = "ldr"
    show Str = "str"



data Operation = Mov 
    | Svc Int 
    | Add 
    | Sub 
    | BL String 
    | Ret 
    | Mul 
    | B (Maybe Condition) 
    | Cmp 
    | Debug 
    | Tst 
    | LdrThenMove
    deriving (Eq)

data Condition = EQ | NE | LT | GE | LE | GT
    deriving (Show, Eq)

instance Show Operation where 
    show Mov = "mov"
    show (Svc i) = "svc #0x0" ++ show i
    show Add = "add"
    show Sub = "sub"
    show (BL s) = "bl _" ++ s
    show Mul = "mul"
    show Ret = "ret"
    show (B (Just c)) = "b" ++ show c 
    show (B Nothing) = "b"
    show Cmp = "cmp"
    show Debug = "DEBUG"
    show Tst = "tst"
    show LdrThenMove = "ldr"


data CodeLine = CL Operation [Operand] | MoffSet MemOp [Operand] | MovePThenMop MemOp [Operand] | MopThenMove MemOp [Operand]


instance Show CodeLine where
    show (CL Debug [StrLit string]) = "\n\t" ++ "DEBUG " ++ string ++ replicate 10 '*' 
    show (CL Cmp [_, a, b]) = "\n\t" ++ "cmp " ++ show a ++ ", " ++ show b 
    show (CL (B (Just c)) [label]) = "\n\t" ++ "b" ++ show c ++ " " ++ show label 
    show (CL (B Nothing) [label]) = "\nb " ++ show label 
    show (CL (BL func) []) = "\n\t" ++ "bl _" ++ func 
    show (CL (Svc i) []) = "\n\tsvc #0x80"
    show (CL LdrThenMove [to, from,  s]) = "\n\t" ++ show LdrThenMove ++ " " ++ show to ++ ", " ++ "[" ++ show from ++ "], " ++ show s 
    show (CL Ret []) = "\n\tret"
    show (CL op opl) = "\n\t" ++ show op ++ " " ++ printOperandList opl 
    show (MoffSet mop [a, b, c]) = "\n\t" ++ show mop ++ " " ++ show a ++ ", [" ++ show b ++", " ++ show c ++ "]" 
    show (MovePThenMop Str [a, b, c]) = "\n\tstr " ++ show a ++ ", [" ++ show b ++", #-" ++ show c ++ "]!" 
    show (MovePThenMop mop [a, b, c]) = "\n\t" ++ show mop ++ " " ++ show a ++ ", [" ++ show b ++", " ++ show c ++ "]!" 
    show (MopThenMove mop [a, b, c]) = "\n\t" ++ show mop ++ " " ++ show a ++ ", [" ++ show b ++"], " ++ show c 
    show _ = "kjadlkjhsdkjhglkjdfh"


printOperandList :: [Operand] -> String 
printOperandList [] = ""
printOperandList [o] = show o 
printOperandList (op:ops) = show op ++ ", " ++ printOperandList ops


type Block = [CodeLine] 

printCfg :: Cfg -> String 
printCfg c =
       let a = Prelude.foldr (\x acc -> acc ++ helper x)  (header ++ "\n\n") $ blocks c  
            where 
            helper :: (String, Block) -> String 
            helper (label, block) =
                "\n"++ label ++ ":\n\t" ++ Prelude.foldr (\cl acc ->  acc ++ show cl) "" block
        in 
        let epilogue = 
                "\n.data \
                \\nnum: .asciz \"%d\\n\" \
                \\n.align 4\
                \\n.text"

        in
        a ++ stdLib ++ epilogue

addLine :: CodeLine -> State (Cfg, Env) Operand
addLine  cl@(CL (BL _) []) = do (cfg, env) <- get
                                put (cfg {insns = cl : insns cfg}, env)  
                                return $ R 0

addLine  cl@(CL _ (op:_)) = get >>= \(cfg, env) ->
                            put (cfg {insns = cl : insns cfg}, env) >>
                            return op

addLine cl@(MopThenMove Ldr (op:_)) = do 
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return op
addLine cl@(MovePThenMop Ldr (op:_)) = do 
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return op
addLine cl@(MoffSet Ldr (op:_)) = do
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return op
addLine cl@(MopThenMove Str _ ) = do
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return $ SP 0
addLine cl@(MovePThenMop Str _ ) = do
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return $ SP 0
addLine cl@(MoffSet Str _ ) = do
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return $ SP 0
addLine  cl = do 
    (cfg, env) <- get 
    put (cfg {insns = cl : insns cfg}, env)  
    return Nop

fixStores :: Env -> Block -> Block 
fixStores env = 
    foldr (\cl acc -> 
        case cl of 
            (MoffSet mop [a, b, Defer s]) ->  
                let index = findIndex (\(var, _) -> var == s) $ deferedStores env ++ vars env in
                case index of 
                    Just i -> MoffSet mop [a, b, Lit (i * 16)] : acc
                    _ -> error $ "variable not found: " ++ s ++ "\nenv: " ++ show env
            _ -> cl : acc
    ) []


fixCfg :: Env -> Cfg -> Cfg 
fixCfg env cfg = 
    let fixedBlocks = foldr (\(s, b) acc -> (s, fixStores env b) : acc) [] $ blocks cfg in
    cfg {blocks = fixedBlocks}

getEnv :: BuildLet Operand -> BuildLet Env 
getEnv (BuildLet o) = BuildLet (\bl -> let (cfg, env, _) = o bl in (cfg, env, env))

startBlock :: String -> State (Cfg, Env) ()
startBlock s  =
    get >>= \(cfg, env) ->
    if currentBlock cfg /= "" then error "end current block before starting a new one" 
    else put (cfg {currentBlock = s}, env)  

endBlock :: State (Cfg, Env) ()
endBlock = 
        get >>= \(cfg, env) ->
        put (Cfg {blocks = (currentBlock cfg, insns cfg): blocks cfg, insns = [],  currentBlock = ""}, env)  

endFunction :: State (Cfg, Env) () 
endFunction =
        get >>= \(cfg, _) ->
        put (Cfg {blocks = (currentBlock cfg, insns cfg): blocks cfg, insns = [],  currentBlock = ""}, emptyEnv)

emptyCfg :: Cfg
emptyCfg = Cfg {blocks=[], insns = [],  currentBlock = ""}




emptyEnv :: Env 
emptyEnv = Env {stack=0, regs= [1,2,3,4,5,6,7,8,9,10,11, 0], vars = [], deferedStores = [], poppedFromStack = [], uid = 0}




