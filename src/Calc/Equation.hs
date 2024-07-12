{- |
Module: calc.Equation
Description: Entry point to parse the equation as a lexer
Copyright: Lucas Elvira Martín, 2024
License: BSD-3-Clause
Maintainer : lucaselvira96@gmail.com
-}
module Calc.Equation
  ( parse
  , Group  
  , Operation(Sumatory, Difference, Multiplication, Division)
  , Equation(..)
  , NumericValue(..)
  )
  where

import Calc.Equation.Internal
import Calc.CustomErrors

{- | Convert a string into a set of Equations

The string is split by space, so it is important to set almost one
between each part of the operation.

If exists any problem converting the string into Group, then raise an Error
-}
parse :: String -> Group
parse input = case parseEquation Nothing $ words input of
  Left  l -> error $ show l
  Right r -> if isValid  r then
               r
             else
               error $ show IsInvalidEquation


