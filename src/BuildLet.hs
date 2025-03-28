module BuildLet where 
import Lib (stdLib)

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

data Env = Env {stack :: Int, regs :: [Reg], vars :: [(String, Operand)], poppedFromStack :: [(String, Operand)], uid :: Int} 
    deriving (Show)

data Operand = Lit Int | StrLit String | R Reg | SP Int | OffSet Int | Nop | LR | AccThenUp Int | SPP
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
    show (OffSet i) = "[SP, " ++ show i ++ "]"
    show (AccThenUp i) = "[SP], " ++ show i
    show SPP = "SP"
    show Nop = ""


data Operation = Mov | Svc Int | Ldr | Add | Str | Sub | BL String | Ret | Mul | B (Maybe Condition) | Cmp | Debug
    deriving (Eq)

data Condition = EQ | NE | LT | GE
    deriving (Show, Eq)

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
    show (B (Just c)) = "b" ++ show c 
    show (B Nothing) = "b"
    show Cmp = "cmp"
    show Debug = "DEBUG"


data CodeLine = CL Operation [Operand] 


instance Show CodeLine where
    show (CL Debug [StrLit string]) = "DEBUG " ++ string ++ replicate 10 '*' ++ "\n\t"
    show (CL Cmp [_, a, b]) = "cmp " ++ show a ++ ", " ++ show b ++ "\n\t"
    show (CL (B (Just c)) [label]) = "b" ++ show c ++ " " ++ show label ++ "\n"
    show (CL (B Nothing) [label]) = "b " ++ show label ++"\n\t"
    show (CL (BL func) []) = "bl _" ++ func ++ "\n\t"
    show (CL (Svc i) []) = "svc #0x80\n\t"
    show (CL Ldr [R r, SP i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "\n\t"   
    show (CL Ldr [R r, OffSet i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP, #" ++ show i ++ "]\n\t"   
    show (CL Ldr [R r, AccThenUp i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "]\n\t"   
    show (CL Ret []) = "ret\n\t"
    show (CL op opl) = show op ++ " " ++ printOperandList opl ++ "\n\t"

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

startBlock :: String -> BuildLet Operand
startBlock s =
    BuildLet (
        \(cfg, env) ->
            if currentBlock cfg /= "" then error "end current block before starting a new one" 
            else
            (cfg {currentBlock = s}, env, Nop)
    )

endBlock :: BuildLet Operand 
endBlock = 
    BuildLet (
        \(cfg, env) ->  (Cfg {blocks = (currentBlock cfg, insns cfg): blocks cfg, insns = [], currentBlock = ""}, env, Nop)
    )

emptyCfg :: Cfg
emptyCfg = Cfg {blocks=[], insns = [], currentBlock = ""}



emptyEnv :: Env 
emptyEnv = Env {stack=0, regs= [1,2,3,4,5,6,7,8,9,10,11,12,13,14, 0], vars = [], poppedFromStack = [], uid = 0}


overWriteEnv :: Env -> BuildLet Operand 
overWriteEnv newEnv = BuildLet (\(cfg , _) -> (cfg, newEnv, Nop))


