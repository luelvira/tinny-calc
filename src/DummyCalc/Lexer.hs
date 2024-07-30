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
  | x == '=' = caseEquals xs'
  | x == ';' = caseIsEndOfFile xs' -- TokEos : lexer xs'
  | isDigit x = caseReadValue xs
  | isOpChar x = caseReadOperator xs
  | isValidAsFirstChar x = caseIsVariable xs
  | otherwise = TokError [x] : lexer xs'
-- -LexerFun

caseReadValue :: String -> [Token]
caseReadValue xs =
  case readValue xs of
    Right (value, rest) -> TokNumber value : lexer rest
    Left _ -> error "Invalid Number"

caseReadOperator :: String -> [Token]
caseReadOperator xs =
  case readOperator xs of
    Right (op, rest) -> TokOperator op : lexer rest
    Left _ -> error "Invalid Operator"

caseIsVariable :: String -> [Token]
caseIsVariable xs =
  case readVariable xs of
    Right (var, rest) -> TokVar var: lexer rest
    Left _ -> error "Invalid variable name"

caseEquals :: String -> [Token]
caseEquals xs =
  case xs of
    ('>':ys) -> TokEquals : lexer ys
    _ -> caseReadOperator xs

caseIsEndOfFile :: String -> [Token]
caseIsEndOfFile (';':_) = [TokEos, TokEof]
caseIsEndOfFile ys = TokEos : lexer ys
