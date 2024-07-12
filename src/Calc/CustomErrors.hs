{- |
Module: calc.CustomErrors
Description: Define some instance of the errors that could be throw while running the library
Copyright: Lucas Elvira Martín, 2024
License: BSD-3-Clause
Maintainer : lucaselvira96@gmail.com
-}
module Calc.CustomErrors where
import Control.Exception

-- | A set of exceptions that could be throw by the program
data MyExceptions
  = IsInvalidNumber -- ^ The element at point is neither a number or operation when should be a number
  | IsInvalidEquation -- ^ There are some errors at the process of convert the operation
  | EmptyValue -- ^ Raise when the program expect some value, but it is missing
  | ErrorsAtProcessing -- ^ Uncontrolled error

instance Show MyExceptions where
  show ex = case ex of
    IsInvalidNumber -> "There are almost one invalid number in the equation"
    IsInvalidEquation -> "The equation is not valid"
    EmptyValue -> "The equation is empty"
    ErrorsAtProcessing -> "There was some errors while read the equation"

instance Exception MyExceptions
