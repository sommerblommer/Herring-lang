module TypedAst where 
import Data.List (uncons) 

predefinedFunctions :: [Function]
predefinedFunctions = [Function {funName ="print", params=[("x", IntType)], body = Scope [], returnType = IntType}]

data Typ = IntType | BoolType | StringType
    deriving (Eq, Show)

data Lit = TLI Int | TLB Bool
    deriving (Show)

data Op = Plus | Minus | Mult
    deriving (Show)

data Expr = 
    Literal {tLit :: Lit} 
    | Ident String 
    | BinOp Expr Op Expr Typ
    | FunCall Expr [Expr] Typ

data Stm = 
    LetIn String Expr Typ
    | Scope [Stm] 
    | Return Expr Typ
    | StmExpr Expr Typ

data Function = Function {funName :: String, params :: [(String, Typ)], body :: Stm, returnType :: Typ}

type TypedAst = [Function]


instance Show Expr where 
    show (Ident s) = s 
    show (Literal {tLit=TLI i}) =  show i
    show (Literal {tLit=TLB i}) =  show i
    show (BinOp l o r t) = "(" ++ show l ++ show o ++ show r ++ ") : " ++ show t
    show (FunCall name args t) = show name ++ "(" ++ foldr (\a acc -> acc ++ show a ++ ", ") "" args ++ "type " ++ show t


prettyPrintTypedAst :: TypedAst -> String 
prettyPrintTypedAst a = "TypedAst:\n" ++ foldl (\acc x -> acc 
                                                ++ "\n" ++ funName x ++ " : " ++ printParams (params x) 
                                                ++ show (returnType x) ++ "\n" 
                                                ++ unlines (fmap ("   " ++) (lines (prettyPrintStm (body x)))) ) "" a

printParams :: [(String, Typ)] -> String 
printParams [] = ""
printParams ((p, pt):xs) = "(" ++ show p ++ " : " ++ show pt ++ ")" ++ " -> " ++ printParams xs

prettyPrintStms :: [Stm] -> String 
prettyPrintStms [] = ""
prettyPrintStms [x] = 
    let str = prettyPrintStm x in 
    let ls =  lines str in 
    let fs = "\9492\9472" ++ head ls in
    let rs = ("  "++) <$> tail ls in
    unlines (fs:rs)
prettyPrintStms (x:xs) = 
    let str = prettyPrintStm x in 
    let ls =  lines str in 
    let fs = "\9500\9472" ++ head ls in
    let rs = ("\9474 " ++) <$> tail ls in
    unlines (fs:rs) ++ prettyPrintStms xs


prettyPrintStm :: Stm -> String 
prettyPrintStm (StmExpr e t) = prettyPrintExpr 0 [0] e ++ " -> " ++ show t
prettyPrintStm (Scope stms) ="Scope\n" ++ prettyPrintStms stms
prettyPrintStm (Return e t) = "Return -> " ++ show t ++ "\n\9492\9472" ++ prettyPrintExpr  2 [] e 
prettyPrintStm (LetIn str ex t) = "let -> " ++ show t ++ "\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr (6 + length str) [] ex 

makeIndents :: Int -> [Int] -> String 
makeIndents _ [] = ""
makeIndents l (x:xs) = replicate (x - l) ' ' ++ "\9474" ++ makeIndents x xs   

makeSpaces :: Int -> [Int] -> String 
makeSpaces a [] = replicate a ' '
makeSpaces a xs = replicate (a - head xs) ' '

prettyPrintExpr :: Int -> [Int] -> Expr -> String
prettyPrintExpr _ _ Literal {tLit=TLI l} = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _ Literal {tLit=TLB l} = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _  (Ident ide) = "ident " ++ ide ++ "\n"
prettyPrintExpr indent xs  (BinOp l o r t) = 
    let start = "BinOp : " ++ show o ++" -> " ++ show t ++ "\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r   
prettyPrintExpr indent xs (FunCall fname args typ) = 
    let start = "FunCall : " ++ "\n" in 
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in 
    let name = "fname: " ++ show fname ++ "\n" in 
    let l = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    let argString = case uncons args  of 
            Nothing -> "()"
            Just (x, []) -> r ++ prettyPrintExpr (indent + 1) xs x 
            Just (x, restArgs) -> 
                let end = r ++ prettyPrintExpr (indent + 1) xs x in
                foldl (\acc arg -> let argStr = prettyPrintExpr (indent + 1) xs arg in 
                                        l ++ argStr ++ acc
                                        ) end restArgs  in
    start ++ l ++  name ++ argString 


