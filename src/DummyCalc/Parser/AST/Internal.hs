{- |
Module: DummyCalc.Parser.AST
Description: AST construction
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com


-}
module DummyCalc.Parser.AST.Internal where

import DummyCalc.Parser.Data.Internal
import DummyCalc.Language as La

-- | Shortcut for the header 2 expressions as parameters and returns a new one.
type Constructor = Expr -> Expr -> Expr

{- |
Given an initial expression, and a list of tuples with operators and expression,
returns a new expression that wrap all the sequence.

The process is recursive, so we need to combine the first 2 expression into only
one, and continue the process until finish the list

-}
-- makeBinOpSeqDef
makeBinOpSeq :: Expr -> [(La.Operation, Expr)] -> Expr
makeBinOpSeq e1 [] = e1
makeBinOpSeq e1 ((op,e2):xs) = makeBinOpSeq (makeBinOp op e1 e2) xs
-- -makeBinOpSeqDef


{- |
Takes a valid binary operator and its left and right operand expression and
returns the corresponding expression. It use association list `assocOpCons' to
associte the valid operator with the Expr constructors.
-}
makeBinOp :: La.Operation -> Constructor
makeBinOp op e1 e2 =
  case lookup op assocOpCons of
    Just c -> c e1 e2
    Nothing -> error ("Invalid operator " <> show op)
  where
    assocOpCons =
      [
        (La.Summatory, Add),
        (La.Difference, Sub),
        (La.Multiplication, Mul),
        (La.Division, Div)
      ]
