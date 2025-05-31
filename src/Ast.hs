module Ast  where 
import Data.List (uncons)

data Lit = LI Int | LB Bool 

instance Show Lit where 
    show (LI i) = show i
    show (LB b) = show b

data Op = Plus | Minus | Mult | Div | Lt | Lte | Gt | Gte | Eq
type Ast = [Function]

type Location = (Int, Int)

data Expr = 
    LitExp Lit Location
    | IExp String Location
    | BinOp Expr Op Expr Location
    | FunCall Expr [Expr] Location -- First expression has to be an ident
    | Range Expr Expr Location
    | Closure Stm  Location
    | ArrLit [Expr] Location
    | ArrLookUp Expr Expr Location

data Stm = 
    LetIn String Expr  Location
    | VarInst String Expr Location
    | ForLoop String Expr Expr Location -- for $string in $iter $body
    | Return Expr Location
    | Scope [Stm] 
    | IfThenElse Expr Expr Expr Location
    | Exp Expr Location

data Function = Function {funName :: String, params :: [(String, String)], body :: Stm, returnType :: String}
    deriving (Show)





-- Rest is for printing

instance Show Op where 
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="
    show Eq = "=="

instance Show Expr where
    show (IExp s _) = s 
    show (LitExp  i _) =  show i
    show (LitExp  b _) =  show b
    show (BinOp l o r _) = "(" ++ show l ++ show o ++ show r ++ ")"
    show (FunCall fname args _) =
        let argString = foldl (\acc e -> show e ++ " " ++ acc) "" args in
        show fname ++ "(" ++ argString ++ ")" 
    show (Range l r _) = show l ++ ".." ++ show r
    show (Closure stm _) = "(" ++ show stm ++ ")" 
    show (ArrLit exps _) = 
        let argString = foldl (\acc e -> show e ++ " " ++ acc) "" exps in
        "[" ++ argString ++ "]" 

    show (ArrLookUp lhs rhs _) = show lhs ++ "[" ++ show rhs ++ "]" 

instance Show Stm where 
    show (LetIn str expr _) = "let " ++ str ++ " = " ++ show expr ++ " in"
    show (VarInst str expr _) = "var " ++ str ++ " = " ++ show expr ++ ";"
    show (ForLoop ident iter body _) = "for " ++ ident ++ " in " ++ show iter ++ " " ++ show body 
    show (Return e _) = "return " ++ show e
    show (Scope stms) = foldl (\acc s -> acc ++ " " ++show s) "" stms
    show (IfThenElse cond th el _) = "if " ++ show cond ++ " then " ++ show th ++ " else " ++ show el
    show (Exp ex _) = show ex


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
prettyPrintStm (Exp e _) = prettyPrintExpr 0 [0] e 
prettyPrintStm (Scope stms) ="Scope\n" ++ stmHelper stms
prettyPrintStm (Return e _) = "Return\n\9492\9472" ++ prettyPrintExpr  2 [] e
prettyPrintStm (LetIn str ex _) = "let\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr 2 [] ex
prettyPrintStm (VarInst str ex _) = "let\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr 2 [] ex
prettyPrintStm (ForLoop ident iter body _) =  "for\n\9500\9472ident : " ++ ident ++ "\n\9500\9472iter\n\9474 \9492\9472" ++ prettyPrintExpr 4 [0] iter ++ "\9500\9472body\n" ++ prettyPrintExpr 4 [0] body
prettyPrintStm (IfThenElse cond th el _) = 
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
prettyPrintExpr _ _ (LitExp  l _) = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _ (LitExp  l _) = "Literal " ++ show l ++ "\n"
prettyPrintExpr _ _  (IExp  id _) = "ident " ++ id ++ "\n"
prettyPrintExpr indent xs (Range l r _) = 
    let start = "Range\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r
prettyPrintExpr indent xs  (BinOp l o r _) = 
    let start = "BinOp : " ++ show o ++"\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r
prettyPrintExpr indent xs (FunCall fname args _) = 
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

prettyPrintExpr _ _ (Closure stm _) = 
    let start = "Closure : " ++ "\n" in 
    start ++ stmHelper [stm]

prettyPrintExpr indent xs (ArrLit exps _) = 
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

prettyPrintExpr indent xs (ArrLookUp lhs rhs _) =
    let start = "ArrLookUp : " ++"\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) lhs 
    ++ r' ++ prettyPrintExpr (indent + 2) xs rhs



    

