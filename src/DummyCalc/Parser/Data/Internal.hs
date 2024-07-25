{- |
Module: DummyCalc.Parser.Data.Internal
Description: Define the types used to build the AST
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com

-}
module DummyCalc.Parser.Data.Internal
  ( ParErr(..)
  , Expr(..)
  ) where

import qualified DummyCalc.Language as La
import DummyCalc.Lexer.Tokens

data ParErr
  = MissingAddOp [Token]
  | MissingMulOp [Token]
  | MissingFactor ParErr [Token]
  | MissingValue [Token]
  | MissingLeftParen [Token]
  | MissingRightParen [Token]
  | NotImplemented String
  deriving (Eq)

instance Show ParErr where
  show (MissingAddOp xs) = "Missing add-like operator at \"" <> show (takeTokens xs) <> "\""
  show (MissingMulOp xs) = "Missing mul-like operator at \"" <> show (takeTokens xs) <> "\""
  show (MissingFactor err xs) = "Missing value or parenthesized expression beginning at \"" <> show (takeTokens xs) <> "\" with nested error [" <> show err <> "]"
  show (MissingValue xs) = "Missing value at \"" <> show (takeTokens xs) <> "\""
  show (MissingLeftParen xs) = "Missing '(' at \"" <> show (takeTokens xs) <> "\""
  show (MissingRightParen xs) = "Missing ')' at \"" <> show (takeTokens xs) <> "\""
  show (NotImplemented f) = "Function: " <> f <> " is not implemented"

-- | AST representation
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var String
  | Val La.Value
  deriving (Eq)

instance Show Expr where
  show (Val v) = show v
  show (Var n) = show n
  show (Add l r) = showPar "+" l r
  show (Sub l r) = showPar "-" l r
  show (Mul l r) = showPar "*" l r
  show (Div l r) = showPar "/" l r

showPar :: String -> Expr -> Expr -> String
-- showPar o e1 e2 = "(" <> show e1 <> " " <> o <> " " <> show e2 <> ")"
showPar o e1 e2 = "(" <> o <> " " <> show e1 <> " " <> show e2 <> " )"

errToken :: Int
errToken = 5

takeTokens :: [Token] -> [Token]
takeTokens = take errToken

