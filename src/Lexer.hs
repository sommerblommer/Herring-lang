module Lexer (lexicalAnalysis, Token(..)) where
import Data.Char (digitToInt)


data Token = 
     LeftParen
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
    | Literal 
    | Ident
    | Return
    deriving (Show, Ord, Eq)

data Content = S String | I Int | Nop

newtype StreamToken a = StreamToken (a, Content)

instance Show a => Show (StreamToken a) where 
    show (StreamToken (a, S s)) = show a ++ " " ++ s 
    show (StreamToken (a, I i)) = show a ++ " " ++ show i 
    show (StreamToken (t, Nop)) = show t 
 
instance Functor StreamToken where 
    fmap f (StreamToken (a, mc)) = StreamToken (f a , mc)
instance Applicative StreamToken where 
    pure a = StreamToken (a, Nop)
    StreamToken (f, _) <*> StreamToken (a, b) = StreamToken (f a, b)

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
lexicalAnalysis :: String -> [StreamToken Token]
lexicalAnalysis =  helper 0 where 
    helper :: Int -> String -> [StreamToken Token]
    helper _ [] = []
    helper toSkip s 
        | null (drop toSkip s) = [] 
        | otherwise = 
            let (x:xs) = drop toSkip s in 
            let Incrementer (token, inc) = findToken x xs in 
            token : helper (toSkip + inc) s




--- >>> findToken 'l' "eta = " 
-- Incrementer (Let,3)
findToken :: Char -> String -> Incrementer (StreamToken Token)
findToken ';' _ = return $ pure SemiColon 
findToken '(' _ = return $ pure LeftParen  
findToken ')' _ = return $ pure RightParen  
findToken '{' _ = return $ pure LeftBracket  
findToken '}' _ = return $ pure RightBreacket  
findToken '=' _ = return $ pure Equal
findToken '+' _ = return $ pure Plus
findToken '-' _ = return $ pure Minus
findToken '\n' (x:xs) = return ' ' >> findToken x xs
findToken '\n' [] = return $ pure EOF
findToken ' ' (x:xs) = return ' ' >> findToken x xs
findToken c xs 
    | c `elem` ['0'..'9'] = identToLit <$> findIdent "" c xs 
    | otherwise = checkIdentForReserved <$> findIdent "" c xs 

identToLit :: StreamToken Token -> StreamToken Token 
identToLit (StreamToken (Ident, S s))= StreamToken (Literal, I $ read s)
identToLit a = a 

checkIdentForReserved :: StreamToken Token -> StreamToken Token 
checkIdentForReserved (StreamToken (Ident, S "let")) = pure Let
checkIdentForReserved (StreamToken (Ident, S "in")) = pure In
checkIdentForReserved (StreamToken (Ident, S "return")) = pure Return
checkIdentForReserved a = a

findIdent :: String -> Char -> String -> Incrementer (StreamToken Token)
findIdent acc c [] = return $ StreamToken (Ident, S $ reverse (c:acc))
findIdent acc ' ' _ = return $ StreamToken (Ident,  S $ reverse acc)
findIdent acc c (x:xs) 
    | x `elem` singleCharTokens = return $ StreamToken ( Ident, S $ reverse (c:acc))
    | otherwise = return c >> findIdent (c:acc) x xs


