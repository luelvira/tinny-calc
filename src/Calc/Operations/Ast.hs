module Calc.Operations.Ast where
import Calc.Lexer.AST
import qualified Calc.Lexer as Lexer


operateWithPriority :: Expr -> Double
operateWithPriority ENull = 0 -- ^ Base case. If the group is empty, returns 0
operateWithPriority (EDig (Lexer.TokNumber  n)) = n -- ^ Base case. If the `group` length is 1, then returns the current value
operateWithPriority (ESum (EDig (Lexer.TokNumber left)) right) = left + operateWithPriority right
operateWithPriority (EDiff (EDig (Lexer.TokNumber left)) right) = left - operateWithPriority right
operateWithPriority (EMult left right) = operateWithPriority left * operateWithPriority right
operateWithPriority (EDiv left right) = operateWithPriority left / operateWithPriority right
