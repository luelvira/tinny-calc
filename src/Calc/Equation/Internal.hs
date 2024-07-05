{-# LANGUAGE InstanceSigs #-}
module Calc.Equation.Internal where

import Data.Maybe

import Calc.CustomErrors

-- | Represent the possibles operations
data Operation = Sumatory | Difference | Division | Multiplication

instance Show Operation where
  show op = case op of
    Sumatory -> "+"
    Difference -> "-"
    Division -> "/"
    Multiplication -> "*"

instance Eq Operation where
  (==) :: Operation -> Operation -> Bool
  (==) op1 op2 = case (op1, op2) of
                   (Sumatory, Sumatory) -> True
                   (Difference, Difference) -> True
                   (Multiplication, Multiplication) -> True
                   (Division, Division) -> True
                   (Sumatory, _) -> False
                   (Difference, _) -> False
                   (Multiplication, _) -> False
                   (Division, _) -> False

-- | Construct a numeric value based on int
newtype NumericValue = NumericValue Int deriving (Show, Eq)

{- | Represent the possibles parts of the equations. It means, the values and the operation

The objective is set them into one common description
-}
data Equation
  = ProcessValue NumericValue
  | ProcessOperation Operation
  deriving (Show, Eq)

-- | A sorted list with the elements of the operation
type Group = [Equation]

{- | Convert a string into a set of Equations

The string is split by space, so it is important to set almost one
between each part of the operation.

If exists any problem converting the string into Group, then raise an Error
-}
parse :: String -> Group
parse input = case parseEquation Nothing $ words input of
  Left l -> error $ show l
  Right r -> if isValid  r then
               r
             else
               error $ show IsInvalidEquation

-- | Convert an array of string into an array of equations.
--
-- If exists any problem, return an instance of MyExceptions
--
-- >>> parseEquation Nothing ["99"]
-- Right $ ProcessValue (NumericValue 99)
-- >>> parseEquation Nothing ["99", "+", "9"]
-- Right $ [(ProcessValue (NumericValue 99)), (ProcessOperation Sumatory), (ProcessValue (NumericValue 9))]
-- >>> parseEquation Nothing []
--  Right []
parseEquation :: Maybe Equation -> [String] -> Either MyExceptions Group
parseEquation context (next:rest) =
  case (parseNext, restEquation) of
    (Right n, Right r) -> Right $ maybe id (:) context (n : r)
    (Left l, _)  -> Left l
    (_, Left l)  -> Left l
  where
    parseNext = case next of
                  "" -> Left EmptyValue
                  "+" -> Right $ ProcessOperation Sumatory
                  "-" -> Right $ ProcessOperation Difference
                  "*" -> Right $ ProcessOperation Multiplication
                  "/" -> Right $ ProcessOperation Division
                  n -> case convertToInt n of
                         Nothing -> Left IsInvalidNumber
                         Just number -> Right number
                         
    restEquation = parseEquation Nothing rest
parseEquation context [] = Right $ maybeToList context

-- | isNumber checks if a string can be converted to Int
isNumber :: String -> Bool
isNumber input = case reads input :: [(Int, String)] of
                   [(_, "")] -> True
                   _         -> False

-- | Check if a set of operations are valid
isValid :: Group -> Bool
isValid ((ProcessValue _):(ProcessOperation _):second:rest) = isValid (second:rest)
isValid ((ProcessValue _):(ProcessValue _):_) = False
isValid [(ProcessValue _), (ProcessOperation _)] = False
isValid [(ProcessValue _)] = True
isValid ((ProcessOperation _):_) = False
isValid [] = True

-- | If string is number, convert to ProcessValue. If not, returns nothing
convertToInt :: String -> Maybe Equation
convertToInt str =
  if isNumber str then
    Just num
  else
    Nothing
  where num = ProcessValue $ NumericValue (read str :: Int)

