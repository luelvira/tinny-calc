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
  , Program(..)
  , StatementList
  , Statement(..)
  , EOF(..)
  , EOS(..)
  ) where

import qualified DummyCalc.Language as La
import DummyCalc.Lexer.Tokens

data ParErr
  = MissingAddOp [Token]
  | MissingMulOp [Token]
  | MissingValue [Token]
  | MissingLeftParen [Token]
  | MissingRightParen [Token]
  | MissingEndOfFile [Token]
  | InvalidAssignExpression [Token]
  | MissingSemiColon     Statement [Token]
  | MissingFactor        ParErr [Token] Int
  | MissingStatementList ParErr [Token] Int
  | InvalidProgram       ParErr [Token] Int
  | NotImplemented String
  deriving (Eq)

instance Show ParErr where
  show (MissingAddOp xs) =
    "MissingAddOp: Missing add-like operator at \"" <> show (takeTokens xs) <> "\""
  show (MissingMulOp xs) =
    "MissingMulOp: Missing mul-like operator at \"" <> show (takeTokens xs) <> "\""
  show (MissingFactor err xs l) =
    "MissingFactor: Missing value or parenthesized expression beginning at \"" <> show
    (takeTokens xs) <> "\" with nested error \n" <> (replicate l '\t') <>
    "[" <> show err <> "]"
  show (MissingValue xs) =
    "MissingValue: Missing value at \"" <> show (takeTokens xs) <> "\""
  show (MissingLeftParen xs) =
    "MissingLeftParen: Missing '(' at \"" <> show (takeTokens xs) <> "\""
  show (MissingRightParen xs) =
    "MissingRightParen: Missing ')' at \"" <> show (takeTokens xs) <> "\""
  show (MissingEndOfFile xs) =
    "MissingEndOfFile: Missing EOF character at \"" <> show (takeTokens xs) <> "\""
  show (MissingStatementList e xs l) =
    "MissingStatementList: Missing statement list at \"" <> show (takeTokens xs) <>
    "\" with nested error \n" <> (replicate l '\t') <> show e
  show (InvalidProgram err xs l) =
    "InvalidProgram: Invalid program expression at \"" <> show (takeTokens xs) <>
    "\" with nested error \n" <> (replicate l '\t') <> show err <> "]"
  show (MissingSemiColon e xs) =
    "MissingSemiColon: Missing semicolon in Statement" <> show e <>
    " at \"" <> show (takeTokens xs) <> "\""
  show (InvalidAssignExpression xs) =
    "InvalidAssignExpression: Invalid assign expression at \"" <> show (takeTokens xs) <> "\""
  show (NotImplemented f) = "Function: " <> f <> " is not implemented"

type StatementList = [Statement]

data Program = Program StatementList deriving (Show, Eq)
data Statement
  = Statement Expr
  | Ass La.Variable Expr
  deriving (Show, Eq)
data EOF = EOF deriving (Show, Eq)
data EOS = EOS deriving (Show, Eq)

-- | AST representation
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var La.Variable
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

