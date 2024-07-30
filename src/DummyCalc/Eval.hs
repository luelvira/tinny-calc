{- |
Module: DummyCalc.Eval
Description: Eval the AST tree module
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com

The AST is the representation of  the current program. The tree
should be evaluate from bottom to top and left to right. To allow it,
the functions need to traverse the tree representation
-}
module DummyCalc.Eval where

import DummyCalc.Parser.Data.Internal
import DummyCalc.Language as La

import DummyCalc.Environment


-- | Eval funciton. Given an Expression, returns the value that results of their operations
evalExpression:: Expr -> Env -> La.NumValue
evalExpression (Val v) _ = v
evalExpression (Var (La.Variable n)) env =
  case getNumValue env n of
    Nothing -> error $ "Undefined variable " <> n
    Just v  -> v

evalExpression (Add l r) env = (+) (evalExpression l env) (evalExpression r env)
evalExpression (Sub l r) env = (-) (evalExpression l env) (evalExpression r env)
evalExpression (Mul l r) env = (*) (evalExpression l env) (evalExpression r env)
evalExpression (Div l r) env = (/) (evalExpression l env) (evalExpression r env)
evalExpression  _ _ = error "Function not implemented"


-- |
-- Eval a program. Given a prgram and a environment, returns a new environment
-- with the values update after executing the instrucctions
eval :: Program -> Env -> Env
eval (Program xs) env = evalStatementList xs env

evalStatementList :: StatementList -> Env -> Env
evalStatementList ((Statement expr):xs) env =
  let v = evalExpression expr env
  in evalStatementList xs (updateNumValue lastNumVal v env)

evalStatementList ((Ass (La.Variable v) e):xs) env =
  evalStatementList xs (setNumValue v (evalExpression e env) env)
evalStatementList (x:_) env = error ("Unexpected error while eval the program at statement " <> show x <> " with env " <> show env)
evalStatementList _ env = env

