{- |
Module: src.DummyCalc.Lexer
Description: Convert the string into a list of tokens
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module DummyCalc.Lexer where

import Data.Char
  ( isSpace
  , isDigit
  )

import DummyCalc.Lexer.Tokens


-- LexerFun
-- | Convert a string into a list of lexems
lexer :: String -> [Token]
lexer "" = []
lexer xs@(x:xs')
  | isSpace x = lexer xs'
  | x == '(' = TokLeftParen : lexer xs'
  | x == ')' = TokRightParen : lexer xs'
  | isDigit x = case readValue xs of
                  Right (value, rest) -> TokNumber value : lexer rest
                  Left _ -> error "Invalid Number"
  | isOpChar x = case readOperator xs of
                   Right (op, rest) -> TokOperator op : lexer rest
                   Left _ -> error "Invalid Operator"
  | otherwise = TokError : lexer xs'
-- -LexerFun
  
