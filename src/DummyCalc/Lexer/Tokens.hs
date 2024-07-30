module DummyCalc.Lexer.Tokens
  ( Token(..)
  , readValue
  , readOperator
  , isOpChar
  , readVariable
  , isValidAsFirstChar
  ) where

import Data.Char (isDigit, isAlphaNum, isAlpha)

import DummyCalc.Lexer.SyntaxError (SyntaxError(..))
import DummyCalc.Language as La

-- TokensDef
-- | Token are the smallest semantically piece of information.
-- | Each constructor represent an abstraction of the readed text
-- | except TokOperator and TokNumber which need information about the values
-- | they represent
data Token
  = TokLeftParen -- ^ (
  | TokRightParen -- ^ )
  | TokEquals -- ^ =
  | TokOperator La.Operation -- ^ + | - | * | /
  | TokNumber La.NumValue -- ^ a number
  | TokVar La.Variable -- ^ a variable
  | TokEof -- ^ End Of File
  | TokEos -- ^ End of Sentence
  | TokError String-- ^ Error
  deriving (Eq)
-- -TokensDef

-- ShowTokens
instance Show Token where
  show TokLeftParen  = "'('"
  show TokRightParen = "')'"
  show TokEquals     = "'='"
  show (TokOperator op) = show op
  show (TokNumber v) = "TN " <> (show v)
  show (TokVar v)    = show v
  show TokEof        = "EOF"
  show TokEos        = "EOS"
  show (TokError char) = "Unrecogniced token at " <> char
-- -ShowTokens

-- opCharsDef
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
-- -opCharsDef

-- stringToDoubleDef
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

readValue :: String -> Either SyntaxError (NumValue, String)
readValue xs = case stringToDouble xs of
                     Right (value, left) -> Right (NumValue value, left)
                     Left l -> Left l
-- -stringToDoubleDef

isValidAsFirstChar :: Char -> Bool
isValidAsFirstChar c = isAlpha c || c == '_'


isValidAsVariable :: Char -> Bool
isValidAsVariable = isAlphaNum


readVariable :: String -> Either SyntaxError (La.Variable, String)
readVariable (x:xs) = if isValidAsFirstChar x then (let
                                                      (name, rest) = span isValidAsVariable xs
                                                    in Right (La.Variable (x:name), rest))
                        else Left InvalidNameVariable
readVariable _ = Left InvalidNameVariable
