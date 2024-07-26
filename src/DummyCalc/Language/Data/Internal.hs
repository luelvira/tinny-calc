{- |
Module: DummyCalc.Language.Data.Internal
Description: Language types definitions
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module DummyCalc.Language.Data.Internal (Value(..), Operation(..),) where

-- | A Data type to store the valid opertions
data Operation
  = Summatory
  | Difference
  | Multiplication
  | Division

instance Show Operation where
  show op = case op of
    Summatory       -> "+"
    Difference      -> "-"
    Multiplication  -> "*"
    Division        -> "/"

instance Eq Operation where
  (==) Summatory Summatory            = True
  (==) Difference Difference          = True
  (==) Multiplication Multiplication  = True
  (==) Division Division              = True
  (==) _ _                            = False

-- | A Data type to store a valid value
newtype Value = Value Double deriving (Eq)
instance Show Value where
  show (Value d) = show d

instance Num Value where
  (+) (Value v1) (Value v2) = Value (v1 + v2)
  (-) (Value v1) (Value v2) = Value (v1 - v2)
  (*) (Value v1) (Value v2) = Value (v1 * v2)
  abs (Value v1) = Value $ abs v1
  signum (Value v1) = Value $ signum v1
  fromInteger = Value . fromInteger

instance Fractional Value where
  (/) (Value v1) (Value v2) = Value $ (/) v1 v2
  fromRational = Value . fromRational

-- DummyCalc/Language/Data/Internal.hs
