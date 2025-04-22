module Ast  where 
import Data.List (uncons)

data Lit = LI Int | LB Bool 

data Op = Plus | Minus | Mult | Lt | Lte | Gt | Gte | Eq
type Ast = [Function]

data Expr = 
    LitExp {lit :: Lit}
    | IExp {ident :: String}
    | BinOp {lhs :: Expr, op :: Op, rhs :: Expr}
    | FunCall Expr [Expr] -- First expression has to be an ident
    | Range Expr Expr
    | Closure Stm 
    | ArrLit [Expr]
    | ArrLookUp Expr Expr

data Stm = 
    LetIn String Expr 
    | VarInst String Expr
    | ForLoop String Expr Expr -- for $string in $iter $body
    | Return Expr
    | Scope [Stm]
    | IfThenElse Expr Expr Expr
    | Exp Expr

data Function = Function {funName :: String, params :: [(String, String)], body :: Stm, returnType :: String}
    deriving (Show)





-- Rest is for printing

instance Show Op where 
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="
    show Eq = "=="

instance Show Expr where
    show IExp {ident=s} = s 
    show LitExp {lit=LI i} =  show i
    show LitExp {lit=LB b} =  show b
    show BinOp {lhs=l, op=o, rhs=r} = "(" ++ show l ++ show o ++ show r ++ ")"
    show (FunCall fname args) =
        let argString = foldl (\acc e -> show e ++ " " ++ acc) "" args in
        show fname ++ "(" ++ argString ++ ")" 
    show (Range l r) = show l ++ ".." ++ show r
    show (Closure stm) = "(" ++ show stm ++ ")" 
    show (ArrLit exps) = 
        let argString = foldl (\acc e -> show e ++ " " ++ acc) "" exps in
        "[" ++ argString ++ "]" 

    show (ArrLookUp lhs rhs) = show lhs ++ "[" ++ show rhs ++ "]" 

instance Show Stm where 
    show (LetIn str expr) = "let " ++ str ++ " = " ++ show expr ++ " in"
    show (VarInst str expr) = "var " ++ str ++ " = " ++ show expr ++ ";"
    show (ForLoop ident iter body) = "for " ++ ident ++ " in " ++ show iter ++ " " ++ show body 
    show (Return e) = "return " ++ show e
    show (Scope stms) = foldl (\acc s -> acc ++ " " ++show s) "" stms
    show (IfThenElse cond th el) = "if " ++ show cond ++ " then " ++ show th ++ " else " ++ show el
    show (Exp ex) = show ex


intToChar :: Int -> Char 
intToChar = toEnum

-- >>> intToChar 128
-- '\128'

prettyPrintAst :: Ast -> String 
prettyPrintAst a = "Ast:\n" ++ foldl (\acc x -> acc 
                                                ++ "\n" ++ funName x ++ " : " ++ printParams (params x) 
                                                ++ returnType x ++ "\n" 
                                                ++ unlines (fmap ("   " ++) (lines (prettyPrintStm (body x)))) ) "" a

printParams :: [(String, String)] -> String 
printParams [] = ""
printParams ((p, pt):xs) = "(" ++ show p ++ " : " ++ show pt ++ ")" ++ " -> " ++ printParams xs

stmHelper :: [Stm] -> String 
stmHelper [] = ""
stmHelper [x] = 
    let str = prettyPrintStm x in 
    let ls =  lines str in 
    let fs = "\9492\9472" ++ head ls in
    let rs = ("  "++) <$> tail ls in
    unlines (fs:rs)
stmHelper (x:xs) = 
    let str = prettyPrintStm x in 
    let ls =  lines str in 
    let fs = "\9500\9472" ++ head ls in
    let rs = ("\9474 " ++) <$> tail ls in
    unlines (fs:rs) ++ stmHelper xs


prettyPrintStm :: Stm -> String 
prettyPrintStm (Exp e) = prettyPrintExpr 0 [0] e 
prettyPrintStm (Scope stms) ="Scope\n" ++ stmHelper stms
prettyPrintStm (Return e) = "Return\n\9492\9472" ++ prettyPrintExpr  2 [] e
prettyPrintStm (LetIn str ex) = "let\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr 2 [] ex
prettyPrintStm (VarInst str ex) = "let\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr 2 [] ex
prettyPrintStm (ForLoop ident iter body) =  "for\n\9500\9472ident : " ++ ident ++ "\n\9500\9472iter\n\9474 \9492\9472" ++ prettyPrintExpr 4 [0] iter ++ "\9500\9472body\n" ++ prettyPrintExpr 4 [0] body
prettyPrintStm (IfThenElse cond th el) = 
    "if\n\9500\9472 " ++ prettyPrintExpr 2 [0] cond 
    ++ "\9500\9472then " ++ prettyPrintExpr 2 [0] th 
    ++ "\9500\9472else " ++ prettyPrintExpr 2 [] el

makeIndents :: Int -> [Int] -> String 
makeIndents _ [] = ""
makeIndents l (x:xs) = replicate (x - l) ' ' ++ "\9474" ++ makeIndents x xs   

makeSpaces :: Int -> [Int] -> String 
makeSpaces a [] = replicate a ' '
makeSpaces a xs = replicate (a - head xs) ' '

prettyPrintExpr :: Int -> [Int] -> Expr -> String
prettyPrintExpr _ _ LitExp {lit= LI l} = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _ LitExp {lit= LB l} = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _  IExp {ident=ide} = "ident " ++ ide ++ "\n"
prettyPrintExpr indent xs (Range l r) = 
    let start = "Range\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r
prettyPrintExpr indent xs  BinOp {lhs=l, op=o, rhs=r} = 
    let start = "BinOp : " ++ show o ++"\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r
prettyPrintExpr indent xs (FunCall fname args) = 
    let start = "FunCall : " ++ "\n" in 
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in 
    let name = "fname: " ++ show fname  in 
    let l = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    let argString = case Data.List.uncons args  of 
            Nothing -> "()"
            Just (x, []) -> "\n" ++ r ++ prettyPrintExpr (indent + 1) xs x 
            Just (x, restArgs) -> 
                let end = r ++ prettyPrintExpr (indent + 1) xs x in
                foldl (\acc arg -> let argStr = prettyPrintExpr (indent + 1) xs arg in 
                                        l ++ argStr ++ acc
                                        ) end restArgs  in
    start ++ l ++  name ++ argString 

prettyPrintExpr _ _ (Closure stm) = 
    let start = "Closure : " ++ "\n" in 
    start ++ stmHelper [stm]

prettyPrintExpr indent xs (ArrLit exps) = 
    let start = "ArrLit : " ++ "\n" in 
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in 
    let l = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    let argString = case Data.List.uncons exps  of 
            Nothing -> "()"
            Just (x, []) -> "\n" ++ r ++ prettyPrintExpr (indent + 1) xs x 
            Just (x, restArgs) -> 
                let end = r ++ prettyPrintExpr (indent + 1) xs x in
                foldl (\acc arg -> let argStr = prettyPrintExpr (indent + 1) xs arg in 
                                        l ++ argStr ++ acc
                                        ) end restArgs  in
    start ++ l ++ argString 

prettyPrintExpr indent xs (ArrLookUp lhs rhs) =
    let start = "ArrLookUp : " ++"\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) lhs 
    ++ r' ++ prettyPrintExpr (indent + 2) xs rhs



    

