{-# LANGUAGE ExistentialQuantification #-}
module Exceptions where 
import Control.Exception (Exception (toException), SomeException)

type Loc = (Int, Int)

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

data TypeException = 
    MissingVar String 
    | TypeE String String Loc  
    | FunCallException String 
    | NotAType String Loc 
    | RecordFieldNameMistmatch String Loc 

instance Show TypeException where 
    show (MissingVar s) = "TypeError: " ++ s
    show (TypeE t1 t2 loc) = "TypeError: type mismatch\nexpected: " ++ t1 ++ "\nactual: " ++ t2 ++ "\nlocation: " ++ show loc
    show (FunCallException s) = "TypeError: " ++ s
    show (NotAType s loc) = "TypeError: " ++ s ++ " is not a type, " ++ show loc
    show (RecordFieldNameMistmatch name loc) = "TypeError: mismatch of names in record literal: \nwrong field name: " ++ show name ++ "\nat: " ++ show loc  
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
