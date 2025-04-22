module Lib
    ( stdLib, 
      llvmStdLib
    ) where

stdLib :: String
stdLib = "\n_print: \
          \ \n\tstr LR, [SP, -16]!\
          \ \n\tmov X1, X0 \
          \ \n\tadrp X0, num@PAGE \ 
          \ \n\tadd X0, X0, num@PAGEOFF \
          \ \n\tstr X1, [SP, -32]! \ 
          \ \n\tbl _printf \ 
          \ \n\tadd SP, SP, 32 \ 
          \ \n\tldr LR, [SP], 16 \ 
          \ \n\tret" 


llvmStdLib :: String 
llvmStdLib = "\ndeclare void @print_integer(i32)\ndeclare i32 @read_integer()\n"
