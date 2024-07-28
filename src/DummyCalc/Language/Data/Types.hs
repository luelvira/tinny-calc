{- |
Module: DummyCalc.Language.Data.Types
Description: Define the primitive types
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com

-}
module DummyCalc.Language.Data.Types where

-- | A Data type to store a valid value
newtype NumValue = NumValue Double deriving (Eq)
instance Show NumValue where
  show (NumValue d) = show d

instance Num NumValue where
  (+) (NumValue v1) (NumValue v2) = NumValue (v1 + v2)
  (-) (NumValue v1) (NumValue v2) = NumValue (v1 - v2)
  (*) (NumValue v1) (NumValue v2) = NumValue (v1 * v2)
  abs (NumValue v1) = NumValue $ abs v1
  signum (NumValue v1) = NumValue $ signum v1
  fromInteger = NumValue . fromInteger

instance Fractional NumValue where
  (/) (NumValue v1) (NumValue v2) = NumValue $ (/) v1 v2
  fromRational = NumValue . fromRational

newtype BoolValue = BoolValue Bool deriving (Eq)
instance Show BoolValue where
  show (BoolValue b) = show b

newtype StringValue = StringValue String deriving (Eq, Show)


data ValType
  = NValue Double
  | BValue Bool
  | SValue String
  deriving (Eq, Show)

defaultNValue :: ValType
defaultNValue = NValue  0

defaultBValue :: ValType
defaultBValue = BValue  False

defaultSValue :: ValType
defaultSValue = SValue  ""

type Value = NumValue

