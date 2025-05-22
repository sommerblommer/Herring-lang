{-# LANGUAGE ExistentialQuantification #-}
module Exceptions where 
import Control.Exception (Exception (toException), SomeException)



data ParseException = forall e . Exception e => ParseException e 
instance Show ParseException where
    show (ParseException e) = "ParseError " ++ show e
instance Exception ParseException 
parseExcetionToException :: Exception e => e -> SomeException 
parseExcetionToException = toException . ParseException


data PExceptions = ShiftReduce String | FindAction String | MissingRule String
    deriving (Show)
instance Exception PExceptions where 
    toException = parseExcetionToException 


data BackendException = forall e . Exception e => BackendException e 
instance Show BackendException where 
    show (BackendException e) = show e
instance Exception BackendException 
backEndExcetionToException :: Exception e => e -> SomeException 
backEndExcetionToException = toException . BackendException

data TypeException = MissingVar String | TypeE String
instance Show TypeException where 
    show (MissingVar s) = "TypeError " ++ show s
    show (MissingVar s) = "TypeError " ++ show s
instance Exception TypeException

