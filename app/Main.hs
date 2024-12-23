module Main (main) where

import Lib
import Lexer (lexicalAnalysis) 
import Parser
import Ast

main :: IO ()
main = do  
    file <- readFile "input.txt"
    print $ lexicalAnalysis file
    parsed <- parse $ lexicalAnalysis file
    putStrLn $ prettyPrintAst parsed 

