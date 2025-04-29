module Main (main) where

import Lexer (lexicalAnalysis) 
import Parser (parse)
import Ast (Ast, prettyPrintAst)
import TypeCheck (typeCheckAst)
import LlvmCodeGen (codegenAst)
import System.Environment (getArgs)
import System.Process

main :: IO ()
main = do  
    args <- getArgs 
    file <- handleArgs args
    -- print $ lexicalAnalysis file
    parsed <- parse $ lexicalAnalysis file
    -- putStrLn $ prettyPrintAst parsed 
    -- putStrLn $ replicate 40 '*'
    let typedAst = typeCheckAst parsed 
    --putStrLn $ prettyPrintTypedAst typedAst
    -- putStrLn "code: "
    let compiled = codegenAst  typedAst
    -- putStrLn compiled 
    writeFile "output.ll" compiled
    callProcess "clang" ["-O0", "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/app/stdlib.c", "output.ll"]
    callProcess "./a.out" []

handleArgs :: [String] -> IO String
handleArgs (filePath:_) = readFile filePath
handleArgs _ = error "only one argument implemented"

    
