{- |
Module: DummyCalc.Lexer.SyntaxError
Description: Syntax Error handel
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}

module DummyCalc.Lexer.SyntaxError where
-- | Errors raised when the parser lexer fails
data SyntaxError
  = InvalidAddopSign
  | InvalidMulopSign
  | InvalidNumber
  | InvalidTermComponent
  | InvalidExpression
  | InvalidOperator
  | EmptyExpression
  | InvalidNameVariable
  deriving (Show, Eq)
