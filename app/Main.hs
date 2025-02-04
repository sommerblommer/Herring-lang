module Main (main) where

import Lib
import Lexer (lexicalAnalysis) 
import Parser (parse)
import Ast (Ast, prettyPrintAst)
import CodeGen
import TypeCheck (typeCheckAst)
import TypedAst 

main :: IO ()
main = do  
    file <- readFile "input.txt"
    print $ lexicalAnalysis file
    parsed <- parse $ lexicalAnalysis file
    putStrLn $ prettyPrintAst parsed 
    putStrLn $ replicate 40 '*'
    let typedAst = typeCheckAst parsed 
    putStrLn $ prettyPrintTypedAst typedAst
    putStrLn "code: "
    let compiled = codegenAst  typedAst
    putStrLn compiled 
    writeFile "output.s" compiled


