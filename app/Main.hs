module Main (main) where

import Lib
import Lexer (lexicalAnalysis) 
import Parser

main :: IO ()
main = do  
    file <- readFile "input.txt"
    print $ lexicalAnalysis file
    testParser
    putStr "bruh"

