module CCodeGen  where 

import TypedAst
import Exceptions
import Control.Exception (throw)
import Data.List (uncons)
import Control.Monad.State 
import Control.Monad.RWS 
import Data.Bifunctor

space :: String -> String -> String 
space a b = a ++ " " ++ b


data Operand = CVar String | CLiteral CLit | Nop
instance Show Operand where 
    show (CVar s) = s 
    show (CLiteral c) = show c 
    show Nop = ""

data CLit = I Int | B Bool
instance Show CLit where 
    show (I i) = show i 
    show (B b) = show b

newtype Env = Env {vars :: [String]}
emptyEnv :: Env 
emptyEnv = Env {vars = []}


data Cfg = Cfg {funcs :: [CFunc], fbuilder :: [CLine]} 

instance Show Cfg where 
    show c = "#include <stdio.h>\n" ++ concatMap show (funcs c)

emptyCfg :: Cfg 
emptyCfg = Cfg {funcs = [], fbuilder = []}

data CType = CIntType | CBoolType | CVoid
instance Show CType where 
    show CIntType = "int"
    show CBoolType = "bool"
    show CVoid = "void"

data CExpr = 
      Op Operand -- op
    | CPlus CExpr CExpr -- cexpr + cexpr
    | FCall String [CExpr] -- string([cexpr])
    | PrintF [(CExpr, CType)]
instance Show CExpr where 
    show (Op o) = show o 
    show (CPlus a b) = show a ++ " + " ++ show b
    show (FCall fn args) = fn ++ "(" ++ foldl (\acc s -> show s ++ ", " ++ acc) "" args ++ ")"
    show (PrintF args) = 
        let mt t = case t of 
                    CIntType -> "%d"
                    CBoolType -> "%d"
                    _ -> error "malformed print"
            (exps, typs) = unzip args 
        in "printf( \"" ++ foldl (\acc a -> mt a ++ acc) "" typs ++ "\""++ foldr (\a acc -> acc ++ "," ++ show a) "" exps ++ ")"
    
data CLine = 
      Decl CType String CExpr 
    | CReturn CExpr
    | ImPure CExpr

instance Show CLine where 
    show cl =  
        let shelper (Decl ct s rhs) = show ct `space` s ++ " = " ++ show rhs 
            shelper (CReturn ce) = "return " ++ show ce 
            shelper (ImPure ce) = show ce
        in shelper cl ++ ";"

data CFunc = CFunc {retType :: CType, cfname :: String, cparams :: [(String, CType)], cbody :: [CLine]}
instance Show CFunc where 
    show cf = 
        let h = show (retType cf) `space` cfname cf 
            pms = foldl (\acc (pname, ptyp) -> show ptyp `space` pname ++ ", " ++ acc) "" $ cparams cf           
            b = foldl (\acc cl -> show cl ++ "\n" ++ acc) "" $ cbody cf
        in h ++ "(" ++ pms ++ ")" ++ "{\n" ++ b ++ "\n}"


createLine :: CLine -> RWS () [String] (Cfg, Env) ()
createLine cl = do 
    (cfg, env) <- get 
    let newCfg = cfg {fbuilder = cl : fbuilder cfg}
    put (newCfg, env)

finalizeFunction ::CType -> String -> [(String, CType)] ->  RWS () [String] (Cfg, Env) () 
finalizeFunction rt name pms = do 
    (cfg, env) <- get 
    let func = CFunc {retType=rt, cfname=name, cparams=pms, cbody= fbuilder cfg}
        newCfg = cfg {funcs = func : funcs cfg, fbuilder = []}
    put (newCfg, env)


lookUpVar :: String -> RWS () [String] (Cfg, Env) Operand 
lookUpVar _ = return Nop

insertVar :: String -> RWS () [String] (Cfg, Env) () 
insertVar v = get >>= \(cfg, env) -> put (cfg, env {vars = v : vars env}) 

concreteType :: Typ -> CType 
concreteType IntType = CIntType 
concreteType BoolType = CBoolType 
concreteType Void = CVoid
concreteType a = error $ "type not implemented in c compiler: " ++ show a

typeOfExpr :: Expr -> CType 
typeOfExpr (Literal {tLit=TLI _}) = CIntType 
typeOfExpr (Literal {tLit=TLB _}) = CBoolType 
typeOfExpr (Ident _ t) = concreteType t
typeOfExpr (FunCall _ _ t) = concreteType t
typeOfExpr _ = error "aaaaa"


codeGenExpr :: Expr -> RWS () [String] (Cfg, Env) CExpr

codeGenExpr  (Literal {tLit=TLI tl}) = return $ Op $ CLiteral $ I tl 

codeGenExpr (Ident s _) = return $ Op $ CVar s

codeGenExpr (FunCall f args _) = do
    let fn = case f of 
             (Ident s _) -> s 
             _ -> throw MalformedFunctionCall

    tell ["function call: " ++ fn]
    if fn == "print" 
        then do 
            a <- mapM codeGenExpr args
            tell ["fcall args: " ++ show a]
            return . PrintF . zip a $ map typeOfExpr args
        else FCall fn <$> mapM codeGenExpr args


codeGenExpr _ = error "expression TODO"

codeGenStm :: Stm -> RWS () [String] (Cfg, Env) () 

codeGenStm (Return expr _ ) = do
    tell ["In statement: return"]
    createLine . CReturn =<< codeGenExpr expr

codeGenStm (LetIn s expr typ) = do 
    tell ["In Statement let in"]
    cexpr <- codeGenExpr expr
    case typ of 
        Void -> createLine $ ImPure cexpr
        _ -> createLine $ Decl (concreteType typ) s cexpr
    

codeGenStm (Scope stms) = mapM_ codeGenStm stms 

codeGenStm _ = error "Statement TODO"

codeGenFunc :: Function -> RWS () [String] (Cfg, Env) () 
codeGenFunc func = do 
    tell ["in function: " ++ funName func]
    _ <- codeGenStm $ body func  
    finalizeFunction (concreteType (returnType func)) (funName func) $ map (second concreteType) $ params func 
    return ()

codeGenAst :: TypedAst -> IO String 
codeGenAst tast = do
    let a = mapM_ codeGenFunc $ fdecls tast
        (_, (cfg, _), w) = runRWS a () (emptyCfg ,emptyEnv)
    mapM_ putStrLn w
    putStr $ show cfg
    return $ show cfg

