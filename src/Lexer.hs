module Lexer (lexicalAnalysis, Token(..)) where
import Data.Char (digitToInt)


data Token = Ident {ident :: String}
    | LeftParen
    | RightParen
    | RightBreacket
    | LeftBracket
    | Equal
    | Plus
    | Minus 
    | SemiColon
    | EOF
    | Let 
    | In
    | Literal {num :: Int}
    deriving (Show, Ord, Eq)

newtype Incrementer a = Incrementer (a, Int)
    deriving (Show)

instance Functor Incrementer where 
 fmap f (Incrementer (a, i)) = Incrementer (f a, i)

instance Applicative Incrementer where 
    pure a = Incrementer (a, 1) 
    Incrementer (f, u) <*> Incrementer (a, i) = Incrementer (f a, i + u) 

instance Monad Incrementer where 
    return = pure
    Incrementer (a, i) >>= f = let Incrementer (b, j) = f a in Incrementer (b, i+j) 

singleCharTokens :: String 
singleCharTokens = ":()=+- ;\n"

--- >>> lexicalAnalysis "main(){\nx = 1;\nreturn x;\n}"
-- [Ident {ident = "main"},LeftParen,RightParen,LeftBracket,Ident {ident = "x"},Equal,Literal {num = 1},SemiColon,Ident {ident = "return"},Ident {ident = "x"},SemiColon,RightBreacket]
lexicalAnalysis :: String -> [Token]
lexicalAnalysis =  helper 0 where 
    helper :: Int -> String -> [Token]
    helper _ [] = []
    helper toSkip s 
        | null (drop toSkip s) = [] 
        | otherwise = 
            let (x:xs) = drop toSkip s in 
            let Incrementer (token, inc) = findToken x xs in 
            token : helper (toSkip + inc) s




--- >>> findToken 'l' "eta = " 
-- Incrementer (Let,3)
findToken :: Char -> String -> Incrementer Token
findToken ';' _ = return SemiColon 
findToken '(' _ = return LeftParen  
findToken ')' _ = return RightParen  
findToken '{' _ = return LeftBracket  
findToken '}' _ = return RightBreacket  
findToken '=' _ = return Equal
findToken '+' _ = return Plus
findToken '-' _ = return Minus
findToken '\n' (x:xs) = return ' ' >> findToken x xs
findToken '\n' [] = return EOF
findToken ' ' (x:xs) = return ' ' >> findToken x xs
findToken c xs 
    | c `elem` ['0'..'9'] = identToLit <$> findIdent "" c xs 
    | otherwise = checkIdentForReserved <$> findIdent "" c xs 

identToLit :: Token -> Token 
identToLit Ident {ident=s}= Literal {num=read s}
identToLit a = a 

checkIdentForReserved :: Token -> Token 
checkIdentForReserved Ident {ident="let"} = Let
checkIdentForReserved Ident {ident="in"} = In
checkIdentForReserved a = a

findIdent :: String -> Char -> String -> Incrementer Token
findIdent acc c [] = return Ident {ident=reverse (c:acc)}
findIdent acc ' ' _ = return Ident {ident=reverse acc}
findIdent acc c (x:xs) 
    | x `elem` singleCharTokens = return Ident {ident=reverse (c:acc)}
    | otherwise = return c >> findIdent (c:acc) x xs


