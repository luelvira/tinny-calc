module Calc where

import qualified Calc.Equation as Eq
import qualified Calc.Operations as Op


process :: String -> Int
process = fromInteger . toInteger . Op.operateWithPriority . Eq.parse


processAST :: String -> Double
processAST = Op.operateWithPriority . Eq.parseAST