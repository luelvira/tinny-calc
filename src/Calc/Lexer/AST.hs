module Calc.Lexer.AST where


import Calc.Lexer.Internal
import Calc.Equation.Internal as Eq

type Name = String

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Name
          | Val Value
          deriving (Eq)

instance Show Expr where
  show (Val v) = show v
  show (Var n) = show n
  show (Add l r) = "(" <> show l <> " + " <> show r <> ")"
  show (Sub l r) = "(" <> show l <> " - " <> show r <> ")"
  show (Mul l r) = "(" <> show l <> " * " <> show r <> ")"
  show (Div l r) = "(" <> show l <> " / " <> show r <> ")"

data ParErr
  = IncompleteError String
  | MissingVal [Token]
  | MissingFactor ParErr [Token]
  | MissingLeftParen [Token]
  | MissingRightParen [Token]
  | MissingAddOperation [Token]
  | MissingMulOperation [Token]
  | InvalidOperator [Token]
  deriving (Eq)


instance Show ParErr where
  show (IncompleteError s)          = "Incomplete expression " <> s
  show (MissingVal xs)              = "Missing value at \"" <> show (getNextTokens xs) <> "\""
  show (MissingFactor err list)     =
    "Missing value or parenthesized expression " <>
    "beginning at \"" <> show (getNextTokens list) <>
    "\" with nested error [" <> show err <> "]"
  show (MissingLeftParen xs)        = "Missing '(' at \"" <> show (getNextTokens xs) <> "\""
  show (MissingRightParen xs)       = "Missing ')' at \"" <> show (getNextTokens xs) <> "\""
  show (MissingAddOperation xs)     = "Missing add-like operator at \"" <> show (getNextTokens xs)
  show (MissingMulOperation xs)     = "Missing mul-like operator at \"" <> show (getNextTokens xs)
  show (InvalidOperator xs'@(x:_))  = "Operator '" <> show x <>
                                      "' is not recogniced at \"" <>
                                      show (getNextTokens xs') <>
                                      "\""
  show (InvalidOperator [])         = "Invalid operator at the end of the expression"
-- | Constant that determinate the number of tokens to be display in case of error
errToken :: Int
errToken = 5

-- | Given a list of tokens, returns the following `errToken` tokens
getNextTokens :: [Token] -> [Token]
getNextTokens = take errToken


-- | <expression> := <term> <moreterm>
parseExpression :: [Token] -> (Either ParErr Expr, [Token])
parseExpression xs =
  case parseTerm xs of
    (Right e1, ys) -> -- <term>
      let (terms, zs) = parseMoreTerm ys
      in (Right (makeBinOpSeq e1 terms), zs)
    (err@(Left _), _) -> (err, xs)

-- | <term> := <factor> <moreFactor>
parseTerm :: [Token] -> (Either ParErr Expr, [Token])
parseTerm xs =
  case parseFactor xs of
    (Right factor, ys) -> -- <factor>
      let (factors, zs) = parseMoreFactor ys
      in (Right (makeBinOpSeq factor factors), zs)
    (err@(Left _), _) -> (err, xs)

-- | Repetition : <moreterms> := (<addterm>)*
parseMoreTerm :: [Token] -> ([(Eq.Operation, Expr)], [Token])
parseMoreTerm xs =
  case parseAddTerm xs of
    (Right (op, ex), ys) -> -- More terms
      let (terms, zs) = parseMoreTerm ys
      in  ((op,ex):terms, zs)
    (Left _, _) -> ([], xs) -- No more terms

parseAddTerm :: [Token] -> (Either ParErr (Eq.Operation, Expr), [Token])
parseAddTerm xs =
  case parseAddOp xs of
    (Right op, ys) ->
      case parseTerm ys of
        (Right ex, zs) -> (Right (op, ex), zs)
        (Left err, _)  -> (Left err, xs)
    (Left err, _) -> (Left err, xs)

parseAddOp :: [Token] -> (Either ParErr Eq.Operation, [Token])
parseAddOp ((TokOperator op):xs)
  | isAddOp op = (Right op, xs)
parseAddOp xs = (Left $ MissingAddOperation xs, xs)


-- | <factor> ::= <val> | <nestexpr>
parseFactor :: [Token] -> (Either ParErr Expr, [Token])
parseFactor xs =
  case parseValue xs of
    r@(Right _, _) -> r -- <val>
    _ ->
      case parseNestExpr xs of
        r@(Right _, _) -> r -- <nestexpr>
        (Left err, remainderTokens) ->
          (Left (MissingFactor err remainderTokens), remainderTokens)

parseMoreFactor :: [Token] -> ([(Eq.Operation,Expr)], [Token])
parseMoreFactor xs =
  case parseMulFactor xs of
    (Right (op,ex), ys) -> let (factors, zs) = parseMoreFactor ys
                           in ((op,ex):factors, zs)
    (Left _, _) -> ([], xs) -- No more factors

parseMulFactor :: [Token] -> (Either ParErr (Eq.Operation, Expr), [Token])
parseMulFactor xs =
  case parseMulOp xs of
    (Right op, ys) ->
      case parseFactor ys of
        (Right ex, zs) -> (Right (op, ex), zs)
        (Left err, _) -> (Left err, xs)
    (Left err, _) -> (Left err, xs)

parseMulOp :: [Token] -> (Either ParErr Eq.Operation, [Token])
parseMulOp ((TokOperator op):xs)
  | isMulOp op = (Right op, xs)
parseMulOp xs = (Left $ MissingMulOperation xs, xs)


-- | <nextexpr> := <leftParen> <expression> <rightParent>
parseNestExpr :: [Token] -> (Either ParErr Expr, [Token])
parseNestExpr xs@(TokLeftParen:ys) = -- <leftParen>
-- Read the rest of the tokens until the next right parenthesis.
  case parseExpression ys of
    (ex@(Right _), zs) ->
      case zs of
        (TokRightParen:as) -> (ex,as) -- return the expression between parenthesis
        _                  -> (Left $ MissingRightParen zs, xs)
    (err@(Left _), _) -> (err, xs) -- No expression
parseNestExpr xs = (Left $ MissingLeftParen xs, xs) -- No left paren

{- | Base case. Given a TokNumber, convert it into a Expr Value
   | Returns a tuple with the either an error or the expression and
   | the list of tokens remainders
-}
parseValue :: [Token] -> (Either ParErr Expr, [Token])
parseValue ((TokNumber n):xs) = (Right (Val n), xs)
parseValue ((TokOperator Eq.Difference):(TokNumber n):xs) = (Right (negateNumber n), xs)
parseValue xs = (Left (MissingVal xs), xs)

negateNumber :: Value -> Expr
negateNumber (RealValue n) = Val $ RealValue (-n)


{- AST CONSTRUCTION -}

{- makeBinOpSeq takes the left operand Expr and a list of pairs ad additional valid binary operators with their corresponding left Expr.

It associates the operations from the right and return the corresponding Expr.

>>> e1 [(+, e2), (-, e3)] = (e1 + (e2 - e3))
-}
makeBinOpSeq :: Expr -> [(Eq.Operation, Expr)] -> Expr
makeBinOpSeq e1 [] = e1
makeBinOpSeq e1 ((op,e2):xs) =
  makeBinOp op e1 (makeBinOpSeq e2 xs)

makeBinOp :: Eq.Operation -> Expr -> Expr -> Expr
makeBinOp Eq.Sumatory e1 e2        = Add e1 e2
makeBinOp Eq.Difference e1 e2      = Sub e1 e2
makeBinOp Eq.Multiplication e1 e2  = Mul e1 e2
makeBinOp Eq.Division e1 e2        = Div e1 e2

