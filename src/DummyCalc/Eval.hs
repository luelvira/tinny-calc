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


-- Eval funciton. Given an Expression, returns the value that results of their operations
eval:: Expr -> La.Value
eval ex =
  case ex of
    (Add ex1 ex2) -> (+) (eval ex1) (eval ex2)
    (Sub ex1 ex2) -> (-) (eval ex1) (eval ex2)
    (Mul ex1 ex2) -> (*) (eval ex1) (eval ex2)
    (Div ex1 ex2) -> (/) (eval ex1) (eval ex2)
    (Val v1) -> v1
    (Var _) -> error "Function not implemented"


