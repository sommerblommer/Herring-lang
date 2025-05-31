module Main (main) where

import Lexer (lexicalAnalysis) 
import Parser (parse)
import Ast (Ast, prettyPrintAst)
import TypeCheck (typeCheckAst)
import LlvmCodeGen (codegenAst)
import System.Environment (getArgs)
import System.Process
import GHC.IO.Exception (ExitCode(ExitFailure))
import Exceptions 
import Control.Exception (throw)

main :: IO ()
main = do  
    args <- getArgs 
    (file, verb) <- handleArgs args
    parsed <- parse verb =<< lexicalAnalysis file
    let typedAst = typeCheckAst parsed 
    let compiled = codegenAst  typedAst
    _ <- if verb then do
            putStrLn $ prettyPrintAst parsed 
            putStrLn $ replicate 40 '*'
            putStrLn compiled 
            else return ()
    writeFile "output.ll" compiled
    (ec, so, se) <- readProcessWithExitCode "clang" ["-O0", "app/stdlib.c", "output.ll"] ""
    case ec of 
        ExitFailure _ -> throw $ LLVME se 
        _ -> return ()
    exitCode <-  waitForProcess =<< spawnProcess "./a.out" []
    case exitCode of 
        ExitFailure _ -> throw $ RE (show ec) "" "" 
        _ -> return ()

handleArgs :: [String] -> IO (String, Bool)
handleArgs (filePath:"verbose":_) = do 
    f <- readFile filePath
    return (f, True)
handleArgs (filePath:_) = do
    f <- readFile filePath
    return (f, False)
handleArgs _ = error "only one argument implemented"


