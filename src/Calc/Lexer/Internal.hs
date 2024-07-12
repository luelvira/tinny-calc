{- |
Module: Calc.Lexer.Internal
Description: Provide the lexer parser for the equations.
Copyright: Lucas Elvira Martín, 2024
License: BSD-3-Clause
Maintainer : lucaselvira96@gmail.com

The lexer will extract the tokens and check if the grammar is valid.

The grammar is defined as 

-- DefineGrammar

@
S := Start symbol
A := add operation
B := {AT}
C := {MF}
F := Factor
M := mult operation
N := number
n := digit
T := term

S -> TB
S -> T
B -> AT
B -> ATB
T -> F
T -> FC
C -> MF
C -> MFC
N -> nN
N -> n
A -> + | -
M -> * | /
@
-- -DefineGrammar
-}

module Calc.Lexer.Internal where
import Data.Char (isDigit)
import Data.Maybe
import Control.Exception


import qualified Calc.Equation.Internal as Eq


-- DefineTokens
-- | A list with the possible tokens readded from the programm
data Token
  = TokLeftParen
  | TokRightParen
  | TokEquals
  | TokDot
  | TokOperator Eq.Operation
  | TokNumber Double
  | TokEof -- End of File
  | TokEos -- End of Sentence
  | TokEol -- End Of Line
  | TokError
  deriving (Show, Eq)
-- -DefineTokens

-- DefineTypes
-- | Addition operations (Sum or difference)
data Addop = Summ | Diff
-- | Multiplication operations (multiplication and division)
data Mulop = Mult | Divi
-- | Real number
newtype Value = RealValue Double deriving (Show, Eq)
-- | Tuple with a Term and a list of possibles additional terms concatenate by an Addop
newtype Expression = Expression (Term, [(Addop, Term)]) deriving (Show)
-- | Tuple with a Factor and a list of possibles addition factors concatenate by an MulOp
newtype Term = Term (Factor, [MulOpFact]) deriving (Show, Eq)
-- | Factor and Value are equivalents
newtype Factor = Factor Value deriving (Show, Eq)
-- | Tuple with an Mulop and a Factor
type MulOpFact = (Mulop, Factor)
-- | Tuple with an Addop and a Term
type AddopTerm = (Addop, Term)
-- -DefineTypes

-- AddProperties
instance Show Addop where
  show Summ = "addop +"
  show Diff = "addop -"

instance Eq Addop where
  (==) Summ Summ = True
  (==) Diff Diff = True
  (==) _ _ = False

instance Show Mulop where
  show Mult = "mulop *"
  show Divi = "mulop /"

instance Eq Mulop where
  (==) Mult Mult = True
  (==) Divi Divi = True
  (==) _ _ = False

-- -AddProperties
data EvalError
  = EvalError
  | InvalidAddopSign
  | InvalidMulopSign
  | InvalidNumber
  | InvalidTermComponent
  | InvalidExpression
  | EmptyExpression
  deriving
    ( Show
    , Eq
    )

instance Exception EvalError


type Group = [Token]
type EvalResult = Either EvalError (Group, String)


-- | Convert a character into the correspondenting value
charToAddop :: Char -> Either EvalError Addop
charToAddop '+' = Right Summ
charToAddop '-' = Right Diff
charToAddop _ = Left InvalidAddopSign

-- | Convert a character into the correspondenting value
charToMulop :: Char -> Either EvalError Mulop
charToMulop '*' = Right Mult
charToMulop '/' = Right Divi
charToMulop _   = Left InvalidMulopSign

-- | Giving a String, transform it into a double.
-- | If the string start with '-', negate the number
-- | If the string start with '.', add a 0 before
stringToValue :: String -> Either EvalError (Value, String)
stringToValue ('-':xs) = case stringToValue xs of
                           Right (RealValue v, rest) -> Right (RealValue (-v), rest)
                           Left _ -> Left InvalidNumber
stringToValue xs'@('.':_) = stringToValue $ '0':xs'
stringToValue xs@(x:_)
  | isDigit x = Right (RealValue (read digitPart :: Double), restPart)
  | otherwise = Left InvalidNumber
  where (digitPart, restPart) = span (\c -> isDigit c  || c == '.') xs
stringToValue "" = Left InvalidNumber


-- | For each character, the function will run all the possibles rules defined
-- | in the grammar building a list of Tokens. The easiest way to do it, is with
-- | a DFS algorith
eval :: String -> Expression
eval str = case expression $ cleanString str of
             Left l -> throw l
             Right r -> r

cleanString :: String -> String
cleanString str = [x | x <- str, x /= ' ']

-- Expression
-- | The first rule should be transform the expression (the initial state) into
-- a <term> or a (<term>, (<addop> <term>)+)
expression :: String -> Either EvalError Expression
expression "" = Left EmptyExpression
expression xs = case evalTerm xs of
                  Left l -> Left l
                  Right (term, addopTerm) -> case evalExpression' Nothing addopTerm of
                                               Right (list, "") -> Right $ Expression (term, list)
                                               Right (_, _:_) -> Left InvalidExpression
                                               Left l -> throw l

-- | Given a list of AddopTerm and a String, read the string while the function returns
-- a new AddopTerm. Once the function can not continue, returns the list with the rest of the
-- string not readed yet
evalExpression' :: Maybe AddopTerm -> String -> Either EvalError ([AddopTerm], String)
evalExpression' context "" = Right (maybeToList context, "")
evalExpression' context (r:rest) =
  let (nextTerm, moreTerm) = case evalTerm rest of
                                Right right -> right
                                Left l -> throw l
      (restAddopTerm, restTerm) = case evalExpression' Nothing moreTerm of
                                    Right rat -> rat
                                    Left l -> throw l
  in
    case r of
      '+' -> Right (maybeToList context <> ((Summ, nextTerm):restAddopTerm), restTerm)
      '-' -> Right (maybeToList context <> ((Diff, nextTerm):restAddopTerm), restTerm)
      _ -> Right ([], restTerm)
-- -Expression

-- EvalTermFunction
-- | Given a string, return a Term and the rest of the string not evaluate
evalTerm :: String -> Either EvalError (Term, String)
evalTerm xs = case evalFactor xs of
                Right (value, rest@(_:_)) -> case evalTerm' Nothing rest of
                                             Right (list, restString)
                                               -> Right (Term (value, list), restString)
                                             Left l -> Left l
                Right (v, rest) -> Right (Term (v, []), rest)
                Left l -> Left l
-- -EvalTermFunction

-- EvalTermFunction'
-- | Given a string and a list of tuples MulOpFact, return the complete list
-- After iterate over the string, and the remainder part
evalTerm' :: Maybe MulOpFact -> String -> Either EvalError ([MulOpFact], String)
evalTerm' context "" = Right (maybeToList context, "")
evalTerm' context rest@(r:rest') =
  let (nextFactor, moreFactor) = case evalFactor rest' of
                                   Right rightPart -> rightPart
                                   Left l  -> throw l
      (restMultOpFact, restTerm) = case evalTerm' Nothing moreFactor of
                         Right rmf -> rmf
                         Left l -> throw l
  in
    case r of
      '*' -> Right (maybeToList context <> ((Mult, nextFactor):restMultOpFact), restTerm)
      '/' -> Right (maybeToList context <> ((Divi, nextFactor):restMultOpFact), restTerm)
      _   -> Right ([], rest)
-- -EvalTermFunction'
  
    
-- EvalFactorFunction
-- | Given a string, returns a Value
evalFactor :: String -> Either EvalError (Factor, String)
evalFactor xs = case stringToValue xs of
                  Right (f,s) -> Right (Factor f, s)
                  Left l -> Left l
-- -EvalFactorFunction

  
