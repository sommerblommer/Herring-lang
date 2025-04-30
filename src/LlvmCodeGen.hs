module LlvmCodeGen (codegenAst) where
import TypedAst
import Lib (llvmStdLib)


data LLtyp = I8 
    | I1 
    | I32
    | Array Int LLtyp -- length and type aka [int x lltyp]
    | Struct String
    | Star LLtyp
    | Ptr
    | Void


instance Show LLtyp where 
    show I8 = "i8"
    show I1 = "i1"
    show I32 = "i32"
    show Ptr  = "ptr " 
    show (Star t) = show t ++ "*"
    show (Array i t) = "[" ++ show i ++ " x " ++ show t ++ "]"
    show (Struct name) = "%struct." ++ name
    show LlvmCodeGen.Void = "void"

tpConvert :: Typ -> LLtyp 
tpConvert IntType = I32
tpConvert BoolType = I1
tpConvert (Pointer t) = Ptr
tpConvert StringType = error "TODO" 
tpConvert TypedAst.Void = LlvmCodeGen.Void 
tpConvert (FunType _) = error "TODO" 

data Bop = Add | Sub | Mul | LLDiv | Eqll | Ltll | Ltell | Gtell | Gtll 
instance Show Bop where 
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show LLDiv = "sdiv"
    show Eqll = "eq" 
    show Ltll = "slt" 
    show Ltell = "sle" 
    show Gtell = "sge" 
    show Gtll = "sgt" 
data Operand = Var String | Lit Int | BLit Bool | Nop
instance Show Operand where 
    show (Var s) = "%" ++ s 
    show (Lit i) = show i 
    show (BLit b) = show b 
    show Nop = ""
data Instruction = 
    LLBinOp Bop LLtyp Operand Operand 
    | Icmp Bop LLtyp Operand Operand 
    | Call LLtyp String [(LLtyp, Operand)]
    | Allocate LLtyp 
    | Store LLtyp Operand Operand
    | Load LLtyp Operand
    | Return LLtyp (Maybe Operand)
    | Br String 
    | Cbr Operand String String 
    | Gep LLtyp [(LLtyp, Operand)]
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
    show (Icmp bop typ op1 op2 ) = "icmp " ++ show bop ++ " " ++ show typ ++ " "++ show op1 ++ ", " ++ show op2
    show (Br label) = "br label %" ++ label
    show (Cbr op branch1 branch2) = "br i1 " ++ show op ++ ", label %" ++ branch1 ++ ", label %" ++ branch2  
    show (Gep retType args) = "getelementptr " ++ show retType ++ foldr (\(typ, op) acc -> ", " ++ show typ ++ " " ++ show op ++ acc) "" args


type Block = [Line]

data Line = Line (Maybe Operand) Instruction
instance Show Line where 
    show (Line (Just op) ins) = show op ++ " = " ++ show ins 
    show (Line Nothing ins) = show ins

data Cfg = Cfg {blocks::[(String, Block)], insns :: [Line], allocas :: [Line], currentBlock :: String}
instance Show Cfg where 
    show cfg = 
        let a = foldl (\acc b -> "\t" ++ show b ++ "\n" ++ acc ) "" 
        in let  handleRest acc [(_, block)] = a block ++ acc
                handleRest acc ((str, block):xs) =  
                    let b = str ++ ":\n" ++ a block ++ acc 
                    in handleRest b xs
                handleRest _ _ = ""
        in handleRest "" $ blocks cfg
        

startBlock :: String -> BuildLet Operand 
startBlock str = BuildLet (\(cfg, env) -> (cfg {currentBlock = str}, env, Nop))

endBlock :: BuildLet Operand 
endBlock = 
    BuildLet (\(cfg, env) -> 
        let newcfg = Cfg {blocks = (currentBlock cfg, insns cfg) : blocks cfg, allocas = allocas cfg, insns = [] ,currentBlock = ""}
        in (newcfg, env, Nop)
    )

data TDecl = TD String [LLtyp]
instance Show TDecl where 
    show  (TD name typs) = "%struct." ++ name ++ " = type { " ++ helper typs ++ " }" 
        where 
            helper [] = "" 
            helper [x] = show x 
            helper (x:xs) = show x ++ ", " ++ helper xs
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

data Program = Program {fdecls :: [FDecl], tdecls :: [TDecl]}
instance Show Program where 
    show p = foldl (\acc a -> show a ++ acc) "" $ fdecls p 

idBuildlet :: BuildLet Operand 
idBuildlet = return Nop

emptyCfg :: Cfg 
emptyCfg = Cfg {blocks = [], insns = [], allocas = [], currentBlock = ""}

data Env = Env {vars :: [(String, Operand)], uid :: Int}

emptyEnv :: Env 
emptyEnv = Env {vars = [], uid = 0}

overWriteEnv :: Env -> BuildLet () 
overWriteEnv e = BuildLet (\(cfg, oldEnv) ->
                    let newEnv = e {uid = uid e + uid oldEnv} -- keep on having a unique number
                    in (cfg, newEnv, ())
                )

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

freshVar :: String -> BuildLet String 
freshVar ident = do 
    i <- getFresh 
    return $ ident ++ show i

addInstruction :: Maybe String -> Instruction -> BuildLet Operand 
addInstruction (Just str) alloc@(Allocate _) = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {allocas = Line (Just (Var fresh)) alloc : allocas cfg}), env, Var fresh))
addInstruction (Just str) ins = do
    fresh <- freshVar str   
    BuildLet (\(cfg, env) -> ((cfg {insns = Line (Just (Var fresh)) ins : insns cfg}), env, Var fresh))

-- Case of no return op
addInstruction _ ins = do 
    BuildLet (\(cfg, env) -> ((cfg {insns = Line Nothing ins : insns cfg}), env, Nop))

typedBopToLLbop :: Op -> Bop 
typedBopToLLbop Plus = Add
typedBopToLLbop Minus = Sub
typedBopToLLbop Mult = Mul
typedBopToLLbop Div = LLDiv
typedBopToLLbop Lt = Ltll 
typedBopToLLbop Lte = Ltell
typedBopToLLbop Gt = Gtll
typedBopToLLbop Gte = Gtell
typedBopToLLbop Eq = Eqll

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
typeOfExpr (ArrLit _ t) = tpConvert t
typeOfExpr (ArrLookUp _ _ t) = tpConvert t
typeOfExpr (Length _ t) = tpConvert t


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
            in (cfg, env, o)
         )
    addInstruction (Just "load") $ Load (tpConvert typ) o

codegenExpr (BinOp l op r typ) = do
    let isCmp Lte = True
        isCmp Lt = True
        isCmp Gte = True
        isCmp Gt = True
        isCmp Eq = True
        isCmp _ = False
    lop <- codegenExpr l
    rop <- codegenExpr r 

    if isCmp op
        then addInstruction (Just "bop") $ Icmp (typedBopToLLbop op) I32 lop rop 
        else addInstruction (Just "bop") $ LLBinOp (typedBopToLLbop op) (tpConvert typ) lop rop 
codegenExpr (FunCall callee args rtyp) = do
    -- Alters the function call of print 
    -- This is to allow for a print function with a polymorphic type 
    -- Might be changed in the future depending on the type system i want to use
    -- Might also cover other functions later on
    let handle_externals "print" [I32] LlvmCodeGen.Void = (Nothing, "print_integer")
        handle_externals "read" [] I32 = (Just "call", "read_integer")
        handle_externals name _ _ = (Just "call", name) 

    let fn = case callee of    
            (Ident str _) -> str 
            _ -> error "malformed function call"
    -- The following code is a mess
    -- because of the control monad and the explicit types of llvm 
    env <- getEnv 
    cfg <- getCfg 
    let (bl, BuildLet grimt) = Prelude.foldr (\arg (cacc, tacc) ->  
                 let code = codegenExpr arg in
                 let t = bruh tacc arg in
                 (cacc >> code, t)
            ) (idBuildlet, return []) args
    _ <- bl
    
    let (_, _, targs) = grimt (cfg, env) 

    let (ret, ffn) = handle_externals fn (map fst targs) (tpConvert rtyp) 

    addInstruction ret $ Call (tpConvert rtyp) ffn targs 

codegenExpr (Range _ _ ) = error "TODO"

codegenExpr (Closure stm _ ) = codegenStm stm

codegenExpr (ArrLit lits typ) = do 
    let t = tpConvert typ
    let arType = Array (length lits) t
    alloc <- addInstruction (Just "alloc") $ Allocate arType
    _ <- fst $ foldl(\(acc, iter) l -> 
                    let code = do 
                            cl <- codegenExpr l 
                            gep <- addInstruction (Just "gep") $ Gep arType [(Ptr, alloc), (I32, Lit 0), (I32, Lit iter)] 
                            addInstruction Nothing $ Store I32 cl gep 
                    in (acc >> code, iter + 1)
                ) (idBuildlet, 0) lits

    BuildLet (\(c, e) -> (c, e, alloc))
    

codegenExpr (ArrLookUp p lup t) = do 
    let typ = tpConvert t  
    ptr <- codegenExpr p 
    lp <- codegenExpr lup 
    gep <- addInstruction (Just "gep") $ Gep (Array 0 typ) [(Ptr, ptr), (I32, Lit 0), (I32, lp)] 
    addInstruction (Just "load") $ Load typ gep 

codegenExpr (Length arg typ) = do
    let t = typeOfExpr arg
    a <- codegenExpr arg
    gep <- addInstruction (Just "lengthGep") $ Gep (Struct "list") [(t, a), (I32, Lit 0), (I32, Lit 0)]
    addInstruction (Just "length") $ Load I32 gep

insertIntoEnv :: String -> Operand -> BuildLet Operand
insertIntoEnv name op = 
    BuildLet (\(cfg, env) -> 
        let newenv = env {vars = (name, op) : vars env }
        in (cfg, newenv, op)
    )

codegenStm :: Stm -> BuildLet Operand
codegenStm (StmExpr _ _ ) = error "TODO"
codegenStm (Scope stms) = 
    foldl (\acc stm -> 
        acc >> codegenStm stm
    ) idBuildlet stms 
codegenStm (TypedAst.Return exp typ) = do
    ret <- codegenExpr exp 
    let rtype = tpConvert typ 
    addInstruction Nothing $ LlvmCodeGen.Return rtype $ Just ret
    
codegenStm (LetIn var expr typ ) = do 
    let t = tpConvert typ 
    rhs <- codegenExpr expr 
    if var == "_" then idBuildlet 
    else do store <- addInstruction (Just (var ++ "ptr")) $ Allocate t 
            _ <- insertIntoEnv var store 
            addInstruction Nothing $ Store t rhs store

codegenStm (IfThenElse cond thn els) = do 
    thenLabel <- freshVar "then"
    elseLabel <- freshVar "else"
    endLabel <- freshVar "endITE"
    env <- getEnv -- save inital env for scoping 
    cndop <- codegenExpr cond 
    _ <- addInstruction Nothing $ Cbr cndop thenLabel elseLabel 
    _ <- endBlock 
    _ <- startBlock thenLabel
    _ <- codegenExpr thn 
    _ <- addInstruction Nothing $ Br endLabel 
    _ <- endBlock 
    _ <- startBlock elseLabel
    _ <- overWriteEnv env
    _ <- codegenExpr els 
    _ <- addInstruction Nothing $ Br endLabel 
    _ <- endBlock 
    startBlock endLabel

codegenStm (ForLoop var iter body typ) = do 
    let helper op = BuildLet (\(cfg, env) -> (cfg, env, op))
    condLabel <- freshVar "cond"
    bodyLabel <- freshVar "body"
    endLabel <- freshVar "end"
    iterVar <- case iter of 
                (Range start end) -> do 
                    alloca <- addInstruction (Just "cond") $ Allocate I32 
                    sop <- codegenExpr start 
                    en <- codegenExpr end 
                    _ <- addInstruction Nothing $ Store I32 sop alloca
                    _ <- addInstruction Nothing $ Br condLabel
                    _ <- insertIntoEnv var alloca
                    _ <- endBlock 
                    _ <- startBlock condLabel
                    load <- addInstruction (Just "cmpLoad") $ Load I32 alloca
                    cmp <- addInstruction (Just "cmp") $ Icmp Ltll I32 load en
                    _ <- addInstruction Nothing $ Cbr cmp bodyLabel endLabel 
                    helper alloca
                _ -> error "Should not be possible"
    _ <- endBlock 
    _ <- startBlock bodyLabel 
    _ <- codegenExpr body 
    l <- addInstruction (Just "load") $ Load I32 iterVar
    add <- addInstruction (Just "plusOne") $ LLBinOp Add I32 l (Lit 1)
    _ <- addInstruction Nothing $ Store I32 add iterVar
    _ <- addInstruction Nothing $ Br condLabel 
    _ <- endBlock 
    startBlock endLabel

storeArgs :: [(String, Typ)] -> BuildLet Operand 
storeArgs = foldr (\(name, typ) acc -> do
                let t = tpConvert typ  
                _ <- acc 
                store <- addInstruction (Just (name ++ "ptr")) $ Allocate t 
                _ <- addInstruction Nothing $ Store t (Var name) store
                insertIntoEnv name store 
            ) idBuildlet 

prependAllocs :: Cfg -> Cfg 
prependAllocs cfg = 
    let (name, b) = last $ blocks cfg in
    let res = b ++ allocas cfg  in
    cfg {blocks = init (blocks cfg) ++ [(name, res)]}

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
    FD (funName fun) (FT typs (tpConvert (returnType fun))) args (prependAllocs cfg) 

codegenAst :: TypedAst -> String  
codegenAst tast = 
    let fds = map codegenFunc tast in 
    let prog = Program {fdecls=fds, tdecls = []} in 
    llvmStdLib ++ show prog
