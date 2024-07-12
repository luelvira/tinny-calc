module Calc.Lexer
  ( lexicalParser
  , Token(..)  
) where

import Data.Char ( isDigit )

import qualified Calc.Equation.Internal as Eq
import Calc.Lexer.Internal


data LexState = LexState
  { lexPos :: Peek
  , lexSource :: String
  , lexContext :: [Token]
  }
  deriving (Show, Eq)

newtype Peek = Peek (Int, Int) deriving (Show, Eq)

move :: Peek -> Peek -> Peek
(move) (Peek (l1, c1)) (Peek (l2, c2)) = Peek (l1 + l2, c1 + c2)
  

-- | Convert the streem into tokens
lexicalParser :: String -> [Token]
lexicalParser src = lex' LexState {
  lexPos = Peek (1, 1),
  lexSource = src,
  lexContext = []
  } src

lex' :: LexState -> String -> [Token]
lex' state "" = lexContext state <> [TokEof]
lex' state src =
  let
    (token, readded) = nextWhileNotWhite "" 0 src
    tokenType = convertToToken token
    _lexContext = lexContext state
    newState = LexState {
      lexContext = _lexContext <> [tokenType],
      lexSource = lexSource state,
      lexPos = lexPos state `move` Peek (0, readded)
      }
  in
    lex' newState (updateSource src readded)


nextWhileNotWhite ::
  String ->
  Int ->
  String ->
  (String, Int)
nextWhileNotWhite "" cursor (' ':xs) = nextWhileNotWhite "" (cursor + 1) xs
nextWhileNotWhite context cursor (' ':_) = (context, cursor)
nextWhileNotWhite context cursor (c:rest) = nextWhileNotWhite (context <> [c]) (cursor + 1) rest
nextWhileNotWhite context cursor "" = (context, cursor)

convertToToken :: String -> Token
convertToToken text
  | text == "(" = TokLeftParen
  | text == ")" = TokRightParen
  | text == "+" = TokOperator Eq.Sumatory
  | text == "-" = TokOperator Eq.Difference
  | text == "/" = TokOperator Eq.Division
  | text == "*" = TokOperator Eq.Multiplication
  | text == ";" = TokEos
  | all isDigit text = TokNumber (read text :: Double)
  | otherwise = TokError
  

updateSource :: String -> Int -> String
updateSource (x:xs) 0 = x:xs
updateSource "" _ = ""
updateSource (_:xs) letters = updateSource xs (letters - 1)
