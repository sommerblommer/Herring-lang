module Main (main) where

import Lib
import Lexer (lexicalAnalysis) 
import Parser
import Ast
import CodeGen
import TypeCheck (typeCheckAst)

main :: IO ()
main = do  
    file <- readFile "input.txt"
    print $ lexicalAnalysis file
    parsed <- parse $ lexicalAnalysis file
    putStrLn $ prettyPrintAst parsed 
    putStrLn $ replicate 40 '*'
    putStrLn "code: "
    putStrLn $ codegenAst $ typeCheckAst parsed


