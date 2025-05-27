{-# LANGUAGE ExistentialQuantification #-}
module Exceptions where 
import Control.Exception (Exception (toException), SomeException)



data ParseException = forall e . Exception e => ParseException e 
instance Show ParseException where
    show (ParseException e) = "ParseError: " ++ show e
instance Exception ParseException 
parseExcetionToException :: Exception e => e -> SomeException 
parseExcetionToException = toException . ParseException


data PExceptions = ShiftReduce String | FindAction String | MissingRule String
instance Show PExceptions where 
    show (ShiftReduce e) = e
    show (FindAction e) = e
    show (MissingRule e) = e
instance Exception PExceptions where 
    toException = parseExcetionToException 


data BackendException = forall e . Exception e => BackendException e 
instance Show BackendException where 
    show (BackendException e) = show e
instance Exception BackendException 
backEndExcetionToException :: Exception e => e -> SomeException 
backEndExcetionToException = toException . BackendException

data TypeException = MissingVar String | TypeE String | FunCallException String
instance Show TypeException where 
    show (MissingVar s) = "TypeError: " ++ s
    show (TypeE s) = "TypeError: " ++ s
    show (FunCallException s) = "TypeError: " ++ s
instance Exception TypeException

data CodeGenException = MissingVarInEnv String | MalformedFunctionCall | TypeNotParsed String
instance Show CodeGenException where 
    show (MissingVarInEnv s) = "CodeGenError: " ++ s
    show MalformedFunctionCall ="CodeGenError: MalformedFunctionCall" 
    show (TypeNotParsed s) ="CodeGenError: Type could not be parsed: " ++ s
instance Exception CodeGenException


data RuntimeError = RE String String String
instance Show RuntimeError where 
    show (RE exitCode stdOut stdErr) = 
        "RuntimeError:\nexitCode: " ++ exitCode
        ++ "\nstdOut: " ++ stdOut 
        ++ "\nstdErr: " ++ stdErr
instance Exception RuntimeError

newtype LLVMException = LLVME String 
instance Show LLVMException where 
    show (LLVME s) = "LLVM: could not compile " ++ s 
instance Exception LLVMException
