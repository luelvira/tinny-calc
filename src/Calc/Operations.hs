module Calc.Operations where

import Calc.Equation
  ( Group
  , Operation (..)
  , Equation (ProcessOperation, ProcessValue)
  , NumericValue (NumericValue)
  )

operateWithPriority :: Group -> Int
operateWithPriority [] = 0
operateWithPriority [ProcessValue (NumericValue n)] = n
operateWithPriority ((ProcessValue (NumericValue n)):(ProcessOperation op):(ProcessValue (NumericValue n2)):xs) =
  case op of
    Sumatory -> n + operateWithPriority (ProcessValue (NumericValue n2):xs)
    Difference -> n - operateWithPriority (ProcessValue (NumericValue n2):xs)
    Multiplication -> operateWithPriority $ ProcessValue (NumericValue (n * n2)):xs
    Division -> operateWithPriority $ ProcessValue (NumericValue (n `div` n2)):xs
operateWithPriority _ = error "Unexpected input"
