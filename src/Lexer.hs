module Lexer (lexicalAnalysis, Token(..), StreamToken(..), Content(..)) where

import Control.Monad.State

data Token = 
     LeftParen
    | RightParen
    | RightBreacket
    | LeftBracket
    | Equal
    | Plus
    | Minus 
    | Slash
    | Star 
    | SemiColon
    | EOF
    | Let 
    | In
    | Var
    | Literal 
    | Ident
    | Return
    | Colon
    | RightArrow
    | Comma
    | If 
    | Then
    | Else
    | Lt 
    | Gt 
    | Lte 
    | Gte 
    | For 
    | LeftArrow
    | Dot
    | LeftSqBracket
    | RightSqBracket
    | Type
    deriving (Show, Ord, Eq)

data Content = Str String | I Int | Nop
    deriving (Show)

type Location = (Int, Int)

data StreamToken = StreamToken { token :: Token, content :: Content, loc :: Location}
    deriving (Show)



singleCharTokens :: String 
singleCharTokens = ":()=+- ;\n,.*[]/<>{}"

lexicalAnalysis :: String -> IO [StreamToken]
lexicalAnalysis =  helper (1,1) where 
    helper :: Location -> String -> IO [StreamToken]
    helper _ [] = return []
    helper startLoc (x:xs) = do
            let (token, (tokenLength, newLoc)) = runState (findToken x xs) (0, startLoc)   
            let tokenLoc = (snd newLoc, fst newLoc - tokenLength)
            -- print newLoc
            -- print $ show token ++ show tokenLength
            rest <- helper newLoc $ drop  tokenLength (x:xs)
            return $ updateLoc tokenLoc token : rest


simpleToken :: Token -> StreamToken 
simpleToken t = StreamToken {token = t,content = Nop, loc = (1,1)}

updateLoc :: Location -> StreamToken -> StreamToken 
updateLoc l st = st {loc = l}

step ::State (Int, Location) () 
step = do
    (len, (cols, loc)) <- get
    put (len + 1, (cols + 1, loc))
newLine :: State (Int, Location) () 
newLine = do 
    (len, (_, lns)) <- get
    put (len + 1, (1, lns + 1))
end :: State (Int, Location) () 
end = put (1000, (0, 0))

stepR :: Token -> State (Int, Location) StreamToken
stepR = (>>) step . return . simpleToken

--- >>> runState (findToken 'h' "ead : ") (1,1) 
-- ((Ident,Str "head",(0,0)),(5,1))
findToken :: Char -> String -> State (Int, Location) StreamToken 
findToken ',' _ = stepR Comma
findToken ';' _ = stepR SemiColon
findToken '(' _ = stepR LeftParen  
findToken ')' _ = stepR RightParen  
findToken '{' _ = stepR LeftBracket  
findToken '}' _ = stepR RightBreacket  
findToken '[' _ = stepR LeftSqBracket  
findToken ']' _ = stepR RightSqBracket  
findToken '=' _ = stepR Equal
findToken '+' _ = stepR Plus
findToken '*' _ = stepR Star
findToken '/' _ = stepR Slash
findToken ':' _ = stepR Colon
findToken '.' _ = stepR Dot
findToken '-' (x:_) 
    | x == '>' = do 
        step
        step
        return $ simpleToken RightArrow
    | otherwise = return $ simpleToken Minus 
findToken '>' (x:y:ys) 
    | x == '=' = step >> stepR Gte
    | otherwise = stepR Gt
findToken '<' (x:y:ys) 
    | x == '=' = step >> stepR Lte
    | x == ' ' = step >> stepR Lt
    | x == '-' = step >> stepR LeftArrow
    | otherwise = checkIdentForReserved <$> findIdent "" '<' (x:y:ys) 
findToken '\n' (x:xs) = newLine >> findToken x xs
findToken '\n' [] =  end >> return (simpleToken EOF)
findToken '\t' (x:xs) = replicateM_ 4 step >> findToken x xs
findToken ' '  (x:xs) = step >> findToken x xs
findToken '-' _ = stepR Minus
findToken c xs 
    | c `elem` ['0'..'9'] = identToLit <$> findIdent "" c xs
    | otherwise = checkIdentForReserved <$> findIdent "" c xs

identToLit :: StreamToken -> StreamToken 
identToLit (StreamToken {token = Ident, content = Str s, loc=l}) = StreamToken {token = Literal, content = I $ read s, loc =  l}
identToLit a = a 

checkIdentForReserved :: StreamToken -> StreamToken 
checkIdentForReserved st 
    | token st /= Ident = st 
    | otherwise         = case content st of 
                          Str "->"     ->    simpleToken RightArrow
                          Str "<-"     ->    simpleToken LeftArrow
                          Str ">="     ->    simpleToken Gte
                          Str "<="     ->    simpleToken Lte
                          Str "<"      ->    simpleToken Lt
                          Str ">"      ->    simpleToken Gt
                          Str "-"      ->    simpleToken Minus
                          Str "let"    ->    simpleToken Let
                          Str "in"     ->    simpleToken In
                          Str "var"    ->    simpleToken Var
                          Str "return" ->    simpleToken Return
                          Str "if"     ->    simpleToken If
                          Str "then"   ->    simpleToken Then
                          Str "else"   ->    simpleToken Else
                          Str "for"    ->    simpleToken For
                          Str "type"   ->    simpleToken Type
                          _            ->    st


findIdent :: String -> Char -> String -> State (Int, Location) StreamToken 
findIdent acc c [] = step >> return StreamToken {token = Ident, content = Str $ reverse (c:acc), loc = (0,0)}
findIdent acc ' ' _ = step >> return StreamToken {token = Ident, content = Str $ reverse acc, loc = (0,0)}
findIdent acc c (x:xs) 
    | x `elem` singleCharTokens = step >> return StreamToken {token = Ident, content = Str $ reverse (c:acc), loc =(0,0)}
    | otherwise = step >> findIdent (c:acc) x xs


