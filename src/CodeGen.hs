module CodeGen where 
import TypedAst
import Data.List (uncons, nub)
import Data.Map as Map
import GHC.Base (build)

type Reg = Int
data Operand = Lit Int | StrLit String | R Reg | SP Int

instance Show Operand where 
    show (Lit i) = "#" ++ show i
    show (StrLit s) = show s 
    show (R r) = "X" ++ show r 
    show (SP i) = "[SP, " ++ "#-" ++ show i ++ "]!"

data Operation = Mov | Svc Int | Ldr | Add | Str | Sub

instance Show Operation where 
    show Mov = "mov"
    show (Svc i) = "svc " ++ show i
    show Ldr = "ldr"
    show Add = "add"
    show Str = "str"
    show Sub = "sub"

data CodeLine = CL Operation [Operand]


instance Show CodeLine where
    show (CL Ldr [R r, SP i]) = show Ldr ++ " " ++ show (R r) ++ ", " ++ "[SP], #" ++ show i ++ "\n\t"   
    show (CL op opl) = show op ++ " " ++ printOperandList opl ++ "\n\t"

printOperandList :: [Operand] -> String 
printOperandList [] = ""
printOperandList [o] = show o 
printOperandList (op:ops) = show op ++ ", " ++ printOperandList ops


type Block = [CodeLine]

newtype Cfg = Cfg {blocks::(String, Block)}
    deriving (Show)

printCfg :: Cfg -> String 
printCfg c =
       let (label, block) = blocks c in 
       header ++ "\n\n" ++label ++ ":\n\t" ++ Prelude.foldr (\cl acc ->  acc ++ show cl) "" block
    -- should be correct when there are multiple blocks
    -- header ++ Prelude.foldr (\(label, block) acc -> label ++ "\n\t" ++ Prelude.foldl (\acc x -> acc ++show x) "" block ++ acc) "" (blocks c)
    

addLine :: CodeLine -> Cfg -> Cfg
addLine cl cfg = 
    let (label, blk) = blocks cfg in 
    Cfg {blocks = (label, cl : blk)}

emptyCfg :: Cfg
emptyCfg = Cfg {blocks=("_start", [])}

getCfg :: Cfg -> [CodeLine] 
getCfg = snd . blocks

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

data Env = Env {stack :: Int, regs :: [Reg], vars :: Map String Operand} 
    deriving (Show)
    
emptyEnv :: Env 
emptyEnv = Env {stack=0, regs= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14], vars = empty}


-- >>> updateStack (R 0) emptyEnv
-- Env {stack = 0, regs = [1,2,3,4,5,6,7,8,9,10,11,12,13,14], vars = fromList []}

updateStack :: Operand -> Env -> Env
updateStack (R r) e = e {regs = if r `elem` regs e then Prelude.filter (/= r) (regs e) else r : regs e}
updateStack (SP _) e = e {stack = 16 + stack e}
updateStack _ e = e

(+>) :: Operand -> Env -> Env 
(+>) = updateStack

-- will pop a free register if there is one or will return an updated stack position
popReg :: Env -> (Env, Operand)
popReg e = let a = uncons $ regs e in 
    case a of 
        Just (r, _) -> (e {regs = tail (regs e)}, R r)
        Nothing -> (updateStack (SP 16) e, SP $ 16 + stack e) 

insertIntoEnv :: String -> Operand -> Env -> Env 
insertIntoEnv s op env = case op of 
        SP _ ->  
            let maxs = stack env in 
            env {vars = insert s (SP (maxs + 16)) $ vars env}
        _ ->  env {vars = insert s op $ vars env}

findMaxOnStack :: Env -> Int 
findMaxOnStack e = 
    Map.foldr (\x acc ->
            case x of 
                SP i -> max i acc
                _ -> acc
    ) 0 $ vars e

-- Creates a new buildlet if operand is an item on the stack 
handleOperand :: Env -> BuildLet Operand -> BuildLet Operand
handleOperand env b@(BuildLet (_, SP i)) = 
    let (_, freeOp) = popReg env in
    b >> BuildLet (addLine (CL Ldr [freeOp, SP i]), freeOp) 
handleOperand _ b = b

testCodeGen :: IO () 
testCodeGen = do 
    let BuildLet (cfg, _) = codegenExpr emptyEnv (BinOp (Literal {tLit = TLI 5}) Plus (Literal {tLit = TLI 4}) IntType)
    print $ cfg emptyCfg

-- This function is for cleaner code and also to ensure that the two function are called with the same environment
codegenAndHandleOp :: Env -> Expr -> BuildLet Operand 
codegenAndHandleOp e = handleOperand e . codegenExpr e


codegenExpr :: Env -> Expr -> BuildLet Operand 
codegenExpr env (Ident str) = case vars env !? str of 
    Just op -> return op 
    Nothing -> error $ "variable " ++ str ++ " not in environment"
codegenExpr env (Literal {tLit=TLI i}) = 
    let op = snd $ popReg env in
    BuildLet (addLine (CL Mov [op, Lit i]), op) 
codegenExpr _ (Literal {tLit=TLB b}) 
    | b =  return (Lit 1)
    | otherwise = return (Lit 0)
codegenExpr env (BinOp l op r _) = do
    let binop = case op of 
            Plus -> Add 
            Minus -> Sub
    lused <- codegenAndHandleOp env l
    let env2 = lused +> env 
    rused <- codegenAndHandleOp env2 r 
    let (_, nextop) = popReg $ rused +> env2
    BuildLet (addLine (CL binop [nextop, lused, rused]), nextop)

codegenStm :: Env ->  Stm -> (BuildLet Operand, Env)
codegenStm env (LetIn ident expr _) =  
    let BuildLet (cfg, b) = do
            a <- codegenAndHandleOp env expr
            let newop = SP $ 16 + findMaxOnStack env
            BuildLet (addLine $ CL Str [a, newop], newop)
    in
    let newenv = insertIntoEnv ident b env in 
    (BuildLet (cfg, b), newenv)
codegenStm env (Return expr _) = 
    let buildlet = do 
            a <- handleOperand env $ codegenExpr env expr 
            let mov_or_load = case a of 
                                SP i -> addLine $ CL Ldr [R 0, SP i]
                                _ -> addLine  $ CL Mov [R 0, a] 
            _ <- BuildLet (mov_or_load, R 0)
            _ <- BuildLet (addLine (CL Mov [R 16, Lit 1]), R 16)
            BuildLet (addLine (CL (Svc 0) [] ), R 0)

    in
    (buildlet, env)
codegenStm _ _ = error  "not implemented yet for stm"

codegenAst :: Ast -> String 
codegenAst stms = 
    let BuildLet (cfg, _) = fst $ Prelude.foldl (\(acc, env) stm -> 
                let (buildlet, newenv) = codegenStm env stm in
                (acc >> buildlet, newenv)
            ) (BuildLet (id, R 0), emptyEnv) stms in
    printCfg $ cfg emptyCfg
    

header :: String 
header = ".global _start\n.align 2\n"






