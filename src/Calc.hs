module Calc where

import qualified Calc.Equation as Eq
import qualified Calc.Operations as Op


process :: String -> Int
process = Op.operateWithPriority . Eq.parse
