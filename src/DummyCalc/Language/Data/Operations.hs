{- |
Module: DummyCalc.Language.Data.Internal
Description: Language types definitions
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module DummyCalc.Language.Data.Operations where

-- | A Data type to store the valid opertions
data Operation
  = Summatory
  | Difference
  | Multiplication
  | Division
  | Assign

instance Show Operation where
  show op = case op of
    Summatory       -> "ADD"
    Difference      -> "SUB"
    Multiplication  -> "MUL"
    Division        -> "DIV"
    Assign          -> "ASS"

instance Eq Operation where
  (==) Summatory Summatory            = True
  (==) Difference Difference          = True
  (==) Multiplication Multiplication  = True
  (==) Division Division              = True
  (==) Assign Assign                  = True
  (==) _ _                            = False


data Variable
  = Variable String
  deriving (Eq)

instance Show Variable where
  show (Variable s) = "let " <> s

