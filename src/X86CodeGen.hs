module X86CodeGen  where
import TypedAst
import Lib (llvmStdLib)
import Exceptions 
import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.State 


data Operand = R Reg | Lit Int | BLit Bool | None

instance Show Operand where 
    show (R r) = show r
    show (Lit i) = show i 
    show (BLit b) = show b 
    show None = ""

data Reg = EAX | EBX | ECX | EDX | ESP | EBP
    deriving (Show)


newtype Env = Env {vars :: [(String, Operand)]}

data Line = 
      Pop Reg 
    | Mov Reg Operand 
    | Ret
instance Show Line where 
    show (Pop r) = "pop " ++ show r 
    show (Mov r op) = "mov " ++ show r ++ ", " ++ show op
    show Ret = "ret"


newtype Cfg = Cfg {instructions :: [Line]}

instance Show Cfg where 
    show cfg = foldl (\acc l -> show l ++ "\n" ++ acc) "" $ instructions cfg

emptyCfg :: Cfg 
emptyCfg = Cfg {instructions = []}

emptyEnv :: Env 
emptyEnv = Env {vars = []}


addInstruction :: Line -> State (Cfg, Env) () 
addInstruction l = do (cfg, e) <- get 
                      put (cfg {instructions = l : instructions cfg}, e)

codegenExpr :: Expr -> State (Cfg, Env) Operand 
codegenExpr Literal {tLit= t} =  
    case t of 
        TLI i -> 
            do 
                _ <- addInstruction $ Mov EAX (Lit i)
                return $ Lit i
        TLB b -> 
            do
                _ <- addInstruction $ Mov EAX (BLit b)
                return $ BLit b
codegenExpr _ = error "TODO!"



codegenStm :: Stm -> State (Cfg, Env) Operand
codegenStm (Return exp t) = do _ <- codegenExpr exp
                               _ <- addInstruction $ Mov ESP (R EBP)
                               _ <- addInstruction $ Pop EBP 
                               _ <- addInstruction Ret
                               return None
codegenStm _ = error "TODO!"

codegenFunc :: Function -> State (Cfg, Env) ()
codegenFunc Function {returnType=rt, params=ps, funName=fname, body=bdy} = 
    void (codegenStm bdy)

codegenAst :: TypedAst -> String 
codegenAst ast =  
    let (cfg, _) = execState (traverse codegenFunc (fdecls ast)) (emptyCfg, emptyEnv)
    in show cfg
    

    
