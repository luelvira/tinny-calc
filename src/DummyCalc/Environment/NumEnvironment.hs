{- |
Module: DummyCalc.Environment.NumEnvironment
Description: 
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com

-}
module DummyCalc.Environment.NumEnvironment where

import DummyCalc.Environment.Internal
import DummyCalc.Language as La

import qualified Data.Map as Map (toList)

type EvalErr = String
type Env = AnEnv La.NumValue

lastNumVal :: Name
lastNumVal = "$"

newNumEnvironment :: Env
newNumEnvironment = insertValue lastNumVal (La.NumValue 0) newEnv

getNumValue :: Env -> Name -> Maybe La.NumValue
getNumValue = getValue

setNumValue :: Name -> La.NumValue -> Env -> Env
setNumValue = insertValue

updateNumValue :: Name -> La.NumValue -> Env -> Env
updateNumValue = updateValue

showNumEnvironment :: Env -> String
showNumEnvironment env =
    concatMap (\(k, v) -> k <> " = " <> show v <> "\n") (Map.toList env)
