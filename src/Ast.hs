module Ast where 

data Lit = LI Int | LB Bool 

data Op = Plus | Minus 
type Ast = [Stm]

data Expr = 
    LitExp {lit :: Lit}
    | IExp {ident :: String}
    | BinOp {lhs :: Expr, op :: Op, rhs :: Expr}

data Stm = 
    LetIn String Expr 
    | Return Expr
    | Scope [Stm]
    | Exp Expr

instance Show Op where 
    show Plus = "+"
    show Minus = "-"

instance Show Expr where
    show IExp {ident=s} = s 
    show LitExp {lit=LI i} =  show i
    show LitExp {lit=LB b} =  show b
    show BinOp {lhs=l, op=o, rhs=r} = "(" ++ show l ++ show o ++ show r ++ ")"

instance Show Stm where 
    show (LetIn str expr) = "let " ++ str ++ " = " ++ show expr ++ " in"
    show (Return e) = "return " ++ show e
    show (Scope stms) = foldl (\acc s -> acc ++ "\n" ++ show s) "" stms
    show (Exp ex) = show ex


intToChar :: Int -> Char 
intToChar = toEnum

-- >>> intToChar 128
-- '\128'

prettyPrintAst :: Ast -> String 
prettyPrintAst a = "Ast:\n" ++ helper a where 
    helper :: [Stm] -> String 
    helper [] = ""
    helper [x] = 
        let str = prettyPrintStm x in 
        let ls =  lines str in 
        let fs = "\9492\9472" ++ head ls in
        let rs = ("  "++) <$> tail ls in
        unlines (fs:rs)
    helper (x:xs) = 
        let str = prettyPrintStm x in 
        let ls =  lines str in 
        let fs = "\9500\9472" ++ head ls in
        let rs = ("\9474 " ++) <$> tail ls in
        unlines (fs:rs) ++ helper xs


prettyPrintStm :: Stm -> String 
prettyPrintStm (Exp e) = prettyPrintExpr 0 [0] e 
prettyPrintStm (Return e) = "Return\n\9492\9472" ++ prettyPrintExpr  2 [] e
prettyPrintStm (LetIn str ex) = "let\n\9500\9472ident " ++ str ++ "\n\9492\9472Exp " ++ prettyPrintExpr (6 + length str) [] ex
prettyPrintStm e = error "prettyprinter not implemented for: " ++ show e 

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
prettyPrintExpr indent xs  BinOp {lhs=l, op=o, rhs=r} = 
    let start = "BinOp : " ++ show o ++"\n" in
    let indents = makeIndents 0 xs in 
    let restSpaces = makeSpaces indent xs in
    let l' = indents ++ restSpaces ++ "\9500" ++  "\9472" in
    let r' = indents ++ restSpaces ++ "\9492" ++  "\9472" in
    start ++ l' ++  prettyPrintExpr (indent + 1) (indent : xs) l 
    ++ r' ++ prettyPrintExpr (indent + 2) xs r
