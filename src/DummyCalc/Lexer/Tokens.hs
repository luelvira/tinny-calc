module DummyCalc.Lexer.Tokens
  ( Token(..)
  , readValue
  , readOperator
  , isOpChar
  ) where

import Data.Char (isDigit)

import DummyCalc.Lexer.SyntaxError (SyntaxError(..))
import DummyCalc.Language.Data.Internal as La

-- | Token are the smallest semantically piece of information.
-- | Each constructor represent an abstraction of the readed text
-- | except TokOperator and TokNumber which need information about the values
-- | they represent
data Token
  = TokLeftParen
  | TokRightParen
  | TokEquals
  | TokOperator La.Operation
  | TokNumber La.Value
  | TokEof
  | TokEos
  | TokEol
  | TokError
  deriving (Eq)


instance Show Token where
  show TokLeftParen  = "'('"
  show TokRightParen = "')'"
  show TokEquals     = "'='"
  show (TokOperator op) = show op
  show (TokNumber v) = show v
  show TokEof        = "EOF"
  show TokEos        = ";"
  show TokEol        = "\\n"
  show TokError      = "Unrecogniced token"

-- | A list with valid character that composes the operations
opChars :: String
opChars = "+-*/~<=>!&|%^"

-- | Function that returns wheter a character is an operation character or not
isOpChar :: Char -> Bool
isOpChar x = x `elem` opChars

-- | Function that, given a String, returns the correspondent Operation
stringToOperator :: String -> Maybe La.Operation
stringToOperator "+" = Just La.Summatory
stringToOperator "-" = Just La.Difference
stringToOperator "*" = Just La.Multiplication
stringToOperator "/" = Just La.Division
stringToOperator _ = Nothing

-- | Read the character at the current position, and returns the right part,
-- | (La.Operation, String) Tuple, if success, and the left part, an SyntaxError, otherwise
readOperator :: String -> Either SyntaxError (La.Operation, String)
readOperator xs = case stringToOperator op of
                    Just o -> Right (o, rest)
                    Nothing -> Left InvalidOperator
                  where
                    (op, rest) = span isOpChar xs

-- | Read a string and returns the number at this position and the remaining
-- | text or an SyntaxError in case of invalid number
stringToDouble :: String -> Either SyntaxError (Double, String)
stringToDouble ('-':xs) = case stringToDouble xs of
                           Right (v, rest) -> Right (-v, rest)
                           Left _ -> Left InvalidNumber
stringToDouble xs'@('.':_) = stringToDouble $ '0':xs'
stringToDouble xs@(x:_)
  | isDigit x = Right (read digitPart :: Double, restPart)
  | otherwise = Left InvalidNumber
  where (digitPart, restPart) = span (\c -> isDigit c  || c == '.') xs
stringToDouble "" = Left InvalidNumber

readValue :: String -> Either SyntaxError (Value, String)
readValue xs = case stringToDouble xs of
                     Right (value, left) -> Right (La.Value value, left)
                     Left l -> Left l

