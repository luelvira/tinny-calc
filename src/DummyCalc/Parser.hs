{- |
Module: DummyCalc.Parser
Description: 
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com


-}
module DummyCalc.Parser
  ( parseExpression
  , parseTerm
  , parseFactor
  , parseVal
  , parseProgram
  )
where

import DummyCalc.Parser.Internal
