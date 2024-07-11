module Calc.Operations where

{-
Uncoment only one of the following imports to select the implementation to use
-}
import Calc.Operations.Lexical
  ( operateWithPriority
  )

import Calc.Operations.Ast
  ( operateWithPriority
  )