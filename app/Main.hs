module Main (main) where

import Lexer (lexicalAnalysis) 
import Parser (parse)
import Ast (Ast, prettyPrintAst)
import TypeCheck (typeCheckAst)
import LlvmCodeGen as  LLVM
import X86CodeGen as X86
import CCodeGen as C
import System.Environment (getArgs)
import System.Process
import GHC.IO.Exception (ExitCode(ExitFailure))
import Exceptions 
import Control.Exception (throw)

main :: IO ()
main = do  
    args <- getArgs 
    (file, verb, extension) <- handleArgs args
    parsed <- parse verb =<< lexicalAnalysis file
    let typedAst = typeCheckAst parsed 
    compiled <- C.codeGenAst typedAst
    _ <- if verb then do
            putStrLn $ prettyPrintAst parsed 
            putStrLn $ replicate 40 '*'
            putStrLn compiled 
            else return ()
    writeFile ("output." ++ extension) compiled
    (ec, so, se) <- readProcessWithExitCode "clang" ["-O0", "app/hstdlib.c", "output." ++ extension] ""
    case ec of 
        ExitFailure _ -> throw $ LLVME se 
        _ -> return ()
    exitCode <-  waitForProcess =<< spawnProcess "./a.out" []
    case exitCode of 
        ExitFailure _ -> throw $ RE (show ec) "" "" 
        _ -> return ()

handleArgs :: [String] -> IO (String, Bool, String)
handleArgs (filePath:"verbose":_) = do 
    f <- readFile filePath
    return (f, True, "c")
handleArgs (filePath:"llvm":_) = do 
    f <- readFile filePath
    return (f, False, "ll")
handleArgs (filePath:_) = do
    f <- readFile filePath
    return (f, False, "c")
handleArgs _ = error "only one argument implemented"


