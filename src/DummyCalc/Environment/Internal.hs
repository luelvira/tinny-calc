{- |
Module: DummyCalc.Environment.Internal
Description: Define the environment where the program will run
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module DummyCalc.Environment.Internal where

import qualified Data.Map as Map

type Name = String

-- |
-- AnEnv is a generic environment type, where the name is the key used and the
-- /a/ is the value that should be returned.
type AnEnv a = Map.Map Name a

newEnv :: AnEnv a
newEnv = Map.empty

getValue :: AnEnv a -> Name -> Maybe a
getValue env n = Map.lookup n env

insertValue :: Name -> a -> AnEnv a -> AnEnv a
insertValue = Map.insert

updateValue :: Name -> a -> AnEnv a -> AnEnv a
updateValue n v env =
    if Map.member n env then
        Map.update (\_ -> Just v) n env
    else
        error ("Attempt to set undefined variable " <> n)
