module Calc.Operations where

import Calc.Equation
  ( Group
  , Operation (..)
  , Equation (ProcessOperation, ProcessValue)
  , NumericValue
  )

-- | function to get the result of the operation using the arithmetic priority.
--
-- If the current operation is a sum or a difference, get the first term and make the operation with the result of the rest of the `Group`
-- If the current operation is a product or division, first get the result of it and continue with the rest of the `Group` converting the new value into the first element of the equation
operateWithPriority :: Group -> NumericValue
operateWithPriority [] = 0 -- ^ Base case. If the group is empty, returns 0
operateWithPriority [ProcessValue  n] = n -- ^ Base case. If the `group` length is 1, then returns the current value

operateWithPriority (
    (ProcessValue n):
    (ProcessOperation op):
    (ProcessValue n2):
    xs
  ) =
  case op of
    Sumatory -> n + operateWithPriority ((ProcessValue n2):xs)
    Difference -> n - operateWithPriority ((ProcessValue n2):xs)
    Multiplication -> operateWithPriority $ ProcessValue (n * n2):xs
    Division -> operateWithPriority $ ProcessValue (n `div` n2):xs
operateWithPriority _ = error "Unexpected input"
