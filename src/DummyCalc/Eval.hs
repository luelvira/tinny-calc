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
eval:: Expr -> Env -> La.NumValue
eval (Val v) _ = v
eval (Var (La.Variable n)) env =
  case getNumValue env n of
    Nothing -> error $ "Undefined variable " <> n
    Just v  -> v

eval (Add l r) env = (+) (eval l env) (eval r env)
eval (Sub l r) env = (-) (eval l env) (eval r env)
eval (Mul l r) env = (*) (eval l env) (eval r env)
eval (Div l r) env = (/) (eval l env) (eval r env)
eval  _ _ = error "Function not implemented"


