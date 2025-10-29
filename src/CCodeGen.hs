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

parenthesis :: String -> String 
parenthesis a = "(" ++ a ++ ")"



data CLit = I Int | B Bool
instance Show CLit where 
    show (I i) = show i 
    show (B b) = show b

newtype Env = Env {vars :: [String]}
emptyEnv :: Env 
emptyEnv = Env {vars = []}


data Cfg = Cfg {funcs :: [CFunc], fbuilder :: [CLine], defered :: [CLine]} 

instance Show Cfg where 
    show c = "#include <stdio.h>\n" ++ concatMap show (funcs c)

emptyCfg :: Cfg 
emptyCfg = Cfg {funcs = [], fbuilder = [], defered = []}

data CType = 
      CIntType 
    | CBoolType 
    | CVoid 
    | CStar CType 
    | CSquare CType
    | CStruct String 

data BOp = CPlus | CMinus | CDiv | CMult | CLT | CLE | CGT | CGE | CEq | PlusEq
concreteOp :: Op -> BOp 
concreteOp Plus = CPlus 
concreteOp Minus = CMinus 
concreteOp Mult = CMult 
concreteOp Div = CDiv 
concreteOp Lt = CLT 
concreteOp Lte = CLE 
concreteOp Gt = CGT 
concreteOp Gte = CGE 
concreteOp Eq = CEq
instance Show BOp where 
    show CPlus = "+"
    show CMinus = "-"
    show CDiv = "/"
    show CMult = "*"
    show CLT = "<"
    show CLE = "<="
    show CGT = ">"
    show CGE = ">="
    show CEq = "=="
    show PlusEq = "+="
instance Show CType where 
    show CIntType = "int"
    show CBoolType = "bool"
    show CVoid = "void"
    show (CStar t ) = show t ++ "*" 
    show (CSquare _) = "[" ++ "]" 
    show (CStruct name) = "struct " ++ name

data CExpr = 
      CVar String -- op
    | CIntLit Int 
    | CBoolLit Bool
    | FCall String [CExpr] -- string([cexpr])
    | PrintF [(CExpr, CType)]
    | CBinOp CExpr BOp CExpr
    | CArrLit [CExpr]
    | CArrLookup CExpr CExpr
    | Malloc (Maybe CType) (Maybe Int) CType  
    | Nop
instance Show CExpr where 
    show Nop = ""
    show (CVar o) = o 
    show (CIntLit i) = show i
    show (CBoolLit b) = show b
    show (FCall fn args) = 
        let helper [] = ""
            helper [x] = show x 
            helper (x:xs) = show x ++ ", " ++ helper xs  
        in fn ++ "(" ++ helper args ++ ")"
    show (PrintF args) = 
        let mt t = case t of 
                    CIntType -> "%d"
                    CBoolType -> "%d"
                    _ -> error "malformed print"
            (exps, typs) = unzip args 
        in "printf( \"" ++ foldl (\acc a -> mt a ++ acc) "" typs ++ "\""++ foldr (\a acc -> acc ++ "," ++ show a) "" exps ++ ")"
    show (CBinOp lhs op rhs) = show lhs `space` show op `space` show rhs
    show (CArrLit ces) = "{" ++ foldl (\acc a -> show a ++ ", " ++ acc) "}" ces 
    show (CArrLookup arr lup) = show arr ++ "[" ++ show lup ++ "]" 
    show (Malloc mc mi ct) = 
        let cast = maybe "" (parenthesis . show) mc 
            i = maybe "" (\a -> show a ++ " * ") mi
            typ = show ct
        in cast ++ " malloc(" ++ i ++ typ ++ ")" 
data CLine = 
      Decl CType String CExpr 
    | CReturn CExpr
    | ImPure CExpr
    | LoopHead CType String CExpr CExpr CExpr 
    | StructAssignment CExpr CExpr
    | StructDecl String [(CType, String)]
    | ScopeStart
    | ScopeEnd

instance Show CLine where 
    show (Decl (CSquare t) s rhs) = show t `space` s ++ "[]" ++ " = " ++ show rhs ++ ";"
    show (Decl ct s rhs) = show ct `space` s ++ " = " ++ show rhs ++ ";"
    show (CReturn ce) = "return " ++ show ce ++ ";"
    show (ImPure ce) = show ce ++ ";"
    show (LoopHead vt vn ve cmp iter) = "for(" ++ show vt `space` vn ++ " = " ++ show ve ++ "; " ++ show cmp ++ "; " ++ show iter  ++ ")"
    show (StructAssignment lhs rhs) = show lhs ++ "->" ++ show rhs 
    show (StructDecl name fields) = "struct " ++ name ++ "{" ++ foldl (\acc (t, fn) -> "\n" ++ show t `space` fn ++ ";" ) "}" fields
    show ScopeStart = "{" 
    show ScopeEnd = "}"


data CFunc = CFunc {retType :: CType, cfname :: String, cparams :: [(String, CType)], cbody :: [CLine]}
instance Show CFunc where 
    show cf = 
        let argList [] = ""
            argList [(pn, pt)] = show pt `space` pn 
            argList ((pname, ptyp):xs) = show ptyp `space` pname ++ ", " ++ argList xs
        in
        let h = show (retType cf) `space` cfname cf 
            pms = argList $ cparams cf           
            b = foldl (\acc cl -> show cl ++ "\n" ++ acc) "" $ cbody cf
        in h ++ "(" ++ pms ++ ")" ++  b 


createLine :: CLine -> RWS () [String] (Cfg, Env) ()
createLine cl = do 
    (cfg, env) <- get 
    let defs = defered cfg
    let newCfg = cfg {fbuilder = defs ++ [cl] ++ fbuilder cfg, defered = []}
    put (newCfg, env)

-- Defers the creation of a line in the CFG till the next createLine call
deferLine :: CLine -> RWS () [String] (Cfg, Env) ()
deferLine cl = do 
    (cfg, env) <- get 
    let newCfg = cfg {defered = cl : defered cfg}
    put (newCfg, env)

finalizeFunction ::CType -> String -> [(String, CType)] ->  RWS () [String] (Cfg, Env) () 
finalizeFunction rt name pms = do 
    (cfg, env) <- get 
    let func = CFunc {retType=rt, cfname=name, cparams=pms, cbody= fbuilder cfg}
        newCfg = cfg {funcs = func : funcs cfg, fbuilder = []}
    put (newCfg, env)


lookUpVar :: String -> RWS () [String] (Cfg, Env) () 
lookUpVar _ = return ()

insertVar :: String -> RWS () [String] (Cfg, Env) () 
insertVar v = get >>= \(cfg, env) -> put (cfg, env {vars = v : vars env}) 

concreteType :: Typ -> CType 
concreteType IntType = CIntType 
concreteType BoolType = CBoolType 
concreteType Void = CVoid
concreteType (Pointer t) = CStar $ concreteType t
concreteType (ArrayType t) = CSquare $ concreteType t
concreteType a = error $ "type not implemented in c compiler: " ++ show a

typeOfExpr :: Expr -> CType 
typeOfExpr (Literal {tLit=TLI _}) = CIntType 
typeOfExpr (Literal {tLit=TLB _}) = CBoolType 
typeOfExpr (Ident _ t) = concreteType t
typeOfExpr (FunCall _ _ t) = concreteType t
typeOfExpr (BinOp _ _ _ t) = concreteType t
typeOfExpr (ArrLit _ t) = concreteType t
typeOfExpr (ArrLookUp _ _ t) = concreteType t
typeOfExpr _ = error "in typeOfExpr"


codeGenExpr :: Expr -> RWS () [String] (Cfg, Env) CExpr

codeGenExpr  (Literal {tLit=TLI tl}) = return $ CIntLit tl 

codeGenExpr (Ident s _) = return $ CVar s

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

codeGenExpr (Closure stm _) = tell ["in closure"] >> codeGenStm stm >> return Nop 

codeGenExpr (BinOp l o r _) = do 
    le <- codeGenExpr l 
    re <- codeGenExpr r
    let bop = concreteOp o
    return $ CBinOp le bop re
 
codeGenExpr (ArrLit args _) = CArrLit . reverse <$> traverse codeGenExpr args

codeGenExpr (ArrLookUp arr lup _) = CArrLookup <$> codeGenExpr arr <*> codeGenExpr lup

codeGenExpr (Length _ _) = error "rethinking length"

codeGenExpr (RecordLit name args) = do 
    tell ["RecordLit for " ++ name]
    mapM_ (\(n, a) ->  deferLine . Decl (typeOfExpr a) n =<< codeGenExpr a) args
    return $ Malloc (Just (CStar (CStruct name))) Nothing (CStruct name)

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
    

codeGenStm (Scope stms) = do 
    tell ["Scope start"] 
    createLine ScopeStart
    mapM_ codeGenStm stms 
    createLine ScopeEnd
    tell ["Scope end"] 

codeGenStm (ForLoop varname iter loopBody _) = do 
    tell ["In for loop"]
    (s, e) <- case iter of 
                (Range start end) -> do 
                                     s <- codeGenExpr start
                                     e <- codeGenExpr end 
                                     return (s,e)
                _ -> error "malformed forloop" 
    let cmp = CBinOp (CVar varname) CLT e
    let i = CBinOp (CVar varname) PlusEq (CIntLit 1)
    cl <- createLine $ LoopHead CIntType varname s cmp i 
    _ <- codeGenExpr loopBody 
    return cl



codeGenStm _ = error "Statement TODO"

codeGenFunc :: Function -> RWS () [String] (Cfg, Env) () 
codeGenFunc func = do 
    tell ["in function: " ++ funName func]
    createLine ScopeStart
    _ <- codeGenStm $ body func  
    createLine ScopeEnd
    finalizeFunction (concreteType (returnType func)) (funName func) $ map (second concreteType) $ params func 

codeGenAst :: TypedAst -> IO String 
codeGenAst tast = do
    let a = mapM_ codeGenFunc $ fdecls tast
        (_, (cfg, _), w) = runRWS a () (emptyCfg ,emptyEnv)
    mapM_ putStrLn w
    putStr $ show cfg
    return $ show cfg

