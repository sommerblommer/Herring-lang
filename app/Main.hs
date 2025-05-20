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
    (file, verb) <- handleArgs args
    parsed <- parse verb $ lexicalAnalysis file
    let typedAst = typeCheckAst parsed 
    let compiled = codegenAst  typedAst
    _ <- if verb then do
            print $ lexicalAnalysis file
            putStrLn $ prettyPrintAst parsed 
            putStrLn $ replicate 40 '*'
            putStrLn compiled 
            else return ()
    writeFile "output.ll" compiled
    callProcess "clang" ["-O0", "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/app/stdlib.c", "output.ll"]
    callProcess "./a.out" []

handleArgs :: [String] -> IO (String, Bool)
handleArgs (filePath:"verbose":_) = do 
    f <- readFile filePath
    return (f, True)
handleArgs (filePath:_) = do
    f <- readFile filePath
    return (f, False)
handleArgs _ = error "only one argument implemented"

    
