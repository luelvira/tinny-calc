module Calc.Lexer.AST where

import qualified Calc.Lexer as Lexer
import qualified Calc.Equation.Internal as Eq

data Expr
  = EDig Lexer.Token
  | ESum Expr Expr
  | EDiff Expr Expr
  | EMult Expr Expr
  | EDiv Expr Expr
  | EParens Lexer.Token Expr Lexer.Token
  | ENull
  deriving Show

newtype ID = ID String


expr ::
  [Lexer.Token] ->
  Expr
expr [] = error "Unexpected end of input"
expr ((Lexer.TokNumber n):(Lexer.TokOperator op):xs)
  = case op of
      Eq.Sumatory -> ESum (EDig $ Lexer.TokNumber n) (expr xs)
      Eq.Difference -> EDiff (EDig $ Lexer.TokNumber n) (expr xs)
      Eq.Multiplication -> EMult (EDig $ Lexer.TokNumber n) (expr xs)
      Eq.Division -> EDiv (EDig $ Lexer.TokNumber n) (expr xs)
expr ((Lexer.TokNumber n):_) = EDig $ Lexer.TokNumber n
expr _ = error "Unexpected input"
