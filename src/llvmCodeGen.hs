module Llvm where 
import TypedAst

data LLtyp = I8 | I1 | I32
    deriving (Show)

tastTypToLLTyp :: Typ -> LLtyp 
tastTypToLLTyp IntType = I32
tastTypToLLTyp BoolType = I1
tastTypToLLTyp StringType = error "TODO" 
tastTypToLLTyp Void = error "TODO" 
tastTypToLLTyp (FunType _) = error "TODO" 

data Bop = Add | Sub | Mul | Div 
    deriving (Show)

data Operand = Var String | Lit Int | BLit Bool | Nop
    deriving (Show)
data Operation = Bop | Store
    deriving (Show)
data Instruction = 
    LLBinOp Bop LLtyp Operand Operand 
    | Call LLtyp Operand [(LLtyp, Operand)]
    | Return LLtyp (Maybe Operand)
    | Br String 
    | Cbr Operand String String 
    deriving (Show)
type Block = [Instruction]

data Cfg = Cfg {blocks::[(String, Block)], insns :: [Instruction], currentBlock :: String}
    deriving (Show)

emptyCfg :: Cfg 
emptyCfg = Cfg {blocks = [], insns = [], currentBlock = ""}

data Env = Env {vars :: [(String, Operand)], uid :: Int}

emptyEnv :: Env 
emptyEnv = Env {vars = [], uid = 0}

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
                let (d, g, h) = b (c, e) in 
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

getFresh :: BuildLet Int 
getFresh = BuildLet (\(cfg, env) -> let fresh = uid env in (cfg, env {uid = fresh + 1}, fresh))

freshVar :: String -> BuildLet Operand 
freshVar ident = do 
    i <- getFresh 
    return $ Var (ident ++ show i)

addInstruction :: String -> Instruction -> BuildLet Operand 
addInstruction str ins = do 
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = ins : insns cfg}), env, fresh))

typedBopToLLbop :: Op -> Bop 
typedBopToLLbop Plus = Add
typedBopToLLbop Minus = Sub
typedBopToLLbop Mult = Mul
typedBopToLLbop Lt = error "TODO"
typedBopToLLbop Lte = error "TODO"
typedBopToLLbop Gt = error "TODO"
typedBopToLLbop Gte = error "TODO"
typedBopToLLbop Eq = error "TODO"

typeOfExpr :: Expr -> LLtyp 
typeOfExpr (BinOp _ _ _ t) = tastTypToLLTyp t
typeOfExpr (Ident _) = error "TODO, Lookup then convert type"
typeOfExpr (Literal {tLit=lit}) = 
    case lit of 
        TLI _ -> I32 
        TLB _ -> I1
typeOfExpr (FunCall _ _ t) = tastTypToLLTyp t
typeOfExpr (Range _ _ ) = error "TODO, what type is a range?"
typeOfExpr (Closure _ t) = tastTypToLLTyp t

getOp :: BuildLet Operand -> Operand 
getOp (BuildLet bl) = let (_, _, op) = bl (emptyCfg, emptyEnv) in op  

codegenExpr :: Expr -> BuildLet Operand 
codegenExpr Literal {tLit=lit} = 
    case lit of  
        TLI i -> return $ Lit i 
        TLB b -> return $ BLit b 
codegenExpr (Ident ident) = error "TODO, Lookup in env" 
codegenExpr (BinOp l op r typ) = do
    lop <- codegenExpr l
    rop <- codegenExpr r 
    addInstruction "bop" $ LLBinOp (typedBopToLLbop op) (tastTypToLLTyp typ) lop rop 
codegenExpr (FunCall callee args rtyp) = do
    let fn = case callee of    
            (Ident str) -> str 
            _ -> error "malformed function call"
    let (bl, targs) = Prelude.foldr (\arg (acc, tops) -> 
                let typ = typeOfExpr arg in
                let code = do
                        _ <- acc
                        codegenExpr arg
                in (code, (typ, getOp code) : tops)
            ) (return Nop, []) args
    _ <- bl -- The generated code of the args 
    addInstruction "call" $ Call (tastTypToLLTyp rtyp) (Var fn) targs
codegenExpr (Range _ _ ) = error "TODO"
codegenExpr (Closure _ _ ) = error "TODO"

codegenStm :: Stm -> BuildLet Operand
codegenStm = error "TODO"

codegenFunc :: Function -> BuildLet Operand
codegenFunc = error "TODO"


codegenAst :: TypedAst -> String  
codegenAst tast = error ""
