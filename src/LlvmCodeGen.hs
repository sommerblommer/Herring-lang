module LlvmCodeGen (codegenAst) where
import TypedAst

data LLtyp = I8 
    | I1 
    | I32
instance Show LLtyp where 
    show I8 = "i8"
    show I1 = "i1"
    show I32 = "i32"

tpConvert :: Typ -> LLtyp 
tpConvert IntType = I32
tpConvert BoolType = I1
tpConvert StringType = error "TODO" 
tpConvert Void = error "TODO" 
tpConvert (FunType _) = error "TODO" 

data Bop = Add | Sub | Mul | Div 
instance Show Bop where 
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show Div = "sdiv"
data Operand = Var String | Lit Int | BLit Bool | Nop
instance Show Operand where 
    show (Var s) = "%" ++ s 
    show (Lit i) = show i 
    show (BLit b) = show b 
    show Nop = ""
data Instruction = 
    LLBinOp Bop LLtyp Operand Operand 
    | Call LLtyp String [(LLtyp, Operand)]
    | Allocate LLtyp 
    | Store LLtyp Operand Operand
    | Load LLtyp Operand
    | Return LLtyp (Maybe Operand)
    | Br String 
    | Cbr Operand String String 
instance Show Instruction where 
    show (LlvmCodeGen.Return typ mop) = 
        let expstr = maybe "" show mop
        in "ret " ++ show typ ++ " " ++ expstr
    show (Store typ op1 op2) = "store " ++ show typ ++ " " ++ show op1 ++ ", ptr " ++ show op2  
    show (Load typ op ) = "load " ++ show typ ++ ", ptr " ++ show op  
    show (Allocate typ) = "alloca " ++ show typ
    show (Call rtyp fname args ) = "call " ++ show rtyp ++ " @" ++ fname ++ "(" ++ argPrinter args ++ ")"
        where 
        argPrinter [] = ""
        argPrinter [(t, n)] = show t ++ " " ++ show n
        argPrinter ((t, n):xs) = show t ++ " " ++ show n ++ ", " ++ argPrinter xs 
    show (LLBinOp bop typ op1 op2 ) = show bop ++ " " ++ show typ ++ " "++ show op1 ++ ", " ++ show op2
    show (Br _) = error "br TODO"
    show (Cbr _ _ _) = error "cbr TODO"
 


type Block = [Line]

data Line = Line (Maybe Operand) Instruction
instance Show Line where 
    show (Line (Just op) ins) = show op ++ " = " ++ show ins 
    show (Line Nothing ins) = show ins

data Cfg = Cfg {blocks::[(String, Block)], insns :: [Line], currentBlock :: String}
instance Show Cfg where 
    show cfg = 
        let a = foldl (\acc b -> "\t" ++ show b ++ "\n" ++ acc ) "" 
        in foldl (\acc (str, block) -> str ++ a block ++ acc ) "" $ blocks cfg 

startBlock :: String -> BuildLet Operand 
startBlock str = BuildLet (\(cfg, env) -> (cfg {currentBlock = str}, env, Nop))

endBlock :: BuildLet Operand 
endBlock = 
    BuildLet (\(cfg, env) -> 
        let newcfg = Cfg {blocks = (currentBlock cfg, insns cfg) : blocks cfg, insns = [] ,currentBlock = ""}
        in (newcfg, env, Nop)
    )

data FType = FT [LLtyp] LLtyp
    deriving (Show)
data FDecl = FD String FType [(String, LLtyp)] Cfg 
instance Show FDecl where
    show (FD fname (FT _ ret) args cfg) = 
        "define " ++ show ret ++ " @" ++ fname ++ "(" ++ helper args ++ ") {\n" ++ show cfg ++ "\n}\n"  
        where 
        helper :: [(String, LLtyp)] -> String 
        helper [] = ""
        helper [(n, t)] = show t ++ "%" ++ n 
        helper ((name, typ):xs) = show typ ++ " %" ++ name ++ ", " ++ helper xs 

newtype Program = Program {fdecls :: [FDecl]}
instance Show Program where 
    show p = foldl (\acc a -> show a ++ acc) "" $ fdecls p 

idBuildlet :: BuildLet Operand 
idBuildlet = return Nop

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
addInstruction str ins@(LLBinOp _ _ _ _) = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = Line (Just fresh) ins : insns cfg}), env, fresh))
addInstruction str ins@(Call _ _ _) = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = Line (Just fresh) ins : insns cfg}), env, fresh))
addInstruction str ins@(Allocate _) = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = Line (Just fresh) ins : insns cfg}), env, fresh))
addInstruction str ins@(Load _ _) = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = Line (Just (fresh)) ins : insns cfg}), env, fresh))
-- Case of no return op
addInstruction str ins = do 
    BuildLet (\(cfg, env) -> ((cfg {insns = Line Nothing ins : insns cfg}), env, Nop))

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
typeOfExpr (BinOp _ _ _ t) = tpConvert t
typeOfExpr (Ident _ t) = tpConvert t 
typeOfExpr (Literal {tLit=lit}) = 
    case lit of 
        TLI _ -> I32 
        TLB _ -> I1
typeOfExpr (FunCall _ _ t) = tpConvert t
typeOfExpr (Range _ _ ) = error "TODO, what type is a range?"
typeOfExpr (Closure _ t) = tpConvert t


getEnv :: BuildLet Env 
getEnv = BuildLet (\(cfg, env) -> (cfg, env, env))
getCfg :: BuildLet Cfg 
getCfg = BuildLet (\(cfg, env) -> (cfg, env, cfg)) 


bruh :: BuildLet [(LLtyp, Operand)] -> Expr -> BuildLet [(LLtyp, Operand)]
bruh acc arg = do
    a <- acc
    BuildLet (\(cfg, env) -> 
        let typ = typeOfExpr arg in
        let BuildLet cop =
                codegenExpr arg
        in let (_, _, op) = cop (cfg, env)
        in (cfg, env, (typ, op):a)
        )
    

codegenExpr :: Expr -> BuildLet Operand 
codegenExpr Literal {tLit=lit} = 
    case lit of  
        TLI i -> return $ Lit i 
        TLB b -> return $ BLit b 

codegenExpr (Ident ident typ) = do 
    o <- BuildLet (\(cfg, env) ->
        let o = case lookup ident (vars env) of
                Just op -> op
                _ -> error $ "could not find var " ++ ident ++ "\nIn env: " ++ show (vars env)
            in 
            (cfg, env, o)
        )
    addInstruction "load" $ Load (tpConvert typ) o

codegenExpr (BinOp l op r typ) = do
    lop <- codegenExpr l
    rop <- codegenExpr r 
    addInstruction "bop" $ LLBinOp (typedBopToLLbop op) (tpConvert typ) lop rop 
codegenExpr (FunCall callee args rtyp) = do
    let fn = case callee of    
            (Ident str _) -> str 
            _ -> error "malformed function call"
    env <- getEnv 
    cfg <- getCfg 
    let (bl, BuildLet grimt) = Prelude.foldr (\arg (cacc, tacc) ->  
                 let code = codegenExpr arg in
                 let t = bruh tacc arg in
                 (cacc >> code, t)
            ) (idBuildlet, return ([])) args
    _ <- bl
    
    let (_, _, targs) = grimt (cfg, env) 

    addInstruction "call" $ Call (tpConvert rtyp) fn targs 
codegenExpr (Range _ _ ) = error "TODO"
codegenExpr (Closure _ _ ) = error "TODO"

insertIntoEnv :: String -> Operand -> BuildLet Operand
insertIntoEnv name op = 
    BuildLet (\(cfg, env) -> 
        let newenv = env {vars = (name, op) : vars env }
        in (cfg, newenv, op)
    )

codegenStm :: Stm -> BuildLet Operand
codegenStm (Scope stms) = 
    foldl (\acc stm -> 
        acc >> codegenStm stm
    ) idBuildlet stms 
codegenStm (TypedAst.Return exp typ) = do
    ret <- codegenExpr exp 
    let rtype = tpConvert typ 
    addInstruction "" $ LlvmCodeGen.Return rtype $ Just ret
    
codegenStm (LetIn var expr typ ) = do 
    let t = tpConvert typ 
    rhs <- codegenExpr expr 
    store <- addInstruction (var ++ "ptr") $ Allocate t 
    _ <- insertIntoEnv var store 
    addInstruction "" $ Store t rhs store
codegenStm _ = error "TODO"

storeArgs :: [(String, Typ)] -> BuildLet Operand 
storeArgs = foldr (\(name, typ) acc -> do
                let t = tpConvert typ  
                _ <- acc 
                store <- addInstruction (name ++ "ptr") $ Allocate t 
                _ <- addInstruction "" $ Store t (Var name) store
                insertIntoEnv name store 
            ) idBuildlet 

codegenFunc :: Function -> FDecl
codegenFunc fun = 
    let args = map (\(name, typ) -> (name, tpConvert typ)) $ params fun in
    let typs = map (\(_, typ) -> tpConvert typ) $ params fun in
    let BuildLet a = do 
            _ <- startBlock "" 
            _ <- storeArgs $ params fun 
            _ <- codegenStm $ body fun
            endBlock 
    in let (cfg, _, _) = a (emptyCfg, emptyEnv) in
    FD (funName fun) (FT typs (tpConvert (returnType fun))) args cfg 


codegenAst :: TypedAst -> String  
codegenAst tast = 
    let fds = map codegenFunc tast in 
    let prog = Program {fdecls=fds} in 
    show prog
