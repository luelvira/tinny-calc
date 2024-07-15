{- |
Module: Calc.Lexer
Description: Provide the lexer parser equations.
Copyright: Lucas Elvira Martín, 2024
License: BSD-3-Clause
Maintainer : lucaselvira96@gmail.com

The lexer will extract the tokens and check if the grammar is valid.
-}
module Calc.Lexer where

import Data.Char
  ( isSpace
  , isDigit
  )

import Calc.Lexer.Internal
import Calc.Lexer.AST

data SyntaxError
  = SyntaxOperator String
  | SyntaxNumber String




--lexicalParser :: String -> Either ParErr Expr
--lexicalParser xs =
--  case lex' xs of
--    [] -> Left $ IncompleteError xs
--    ts ->
--      case parseExpression ts of
--

lex' :: String -> [Token]
lex' "" = []
lex' xs@(x:xs')
  | isSpace x = lex' xs' -- in case the string have not been cleaned
  | x == '(' = TokLeftParen : lex' xs'
  | x == ')' = TokRightParen : lex' xs'
  | isDigit x = case stringToValue xs of
                  Right (value, rest) -> TokNumber value : lex' rest
                  Left _ -> error "There was an error reading a number"
  | isOpChar x = let
      (opString, rest) = span isOpChar xs
      op = case stringToOperator opString of
             Just o -> o
             Nothing -> error ("There was an error reading the operation " <> opString)
      in TokOperator op : lex' rest
  | otherwise = TokError : lex' xs'

