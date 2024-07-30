{- |
Module: DummyCalc.Parser.Internal
Description: Module in charge of build the AST from the tokens list
Copyright: (C) Lucas Elvira Martín <lucaselvira96@gmail.com>, 2024
License: GPL-3
Maintainer: lucaselvira96@gmail.com
-}
module DummyCalc.Parser.Internal where

import DummyCalc.Language as La
import DummyCalc.Parser.Data.Internal
import DummyCalc.Lexer.Tokens
import DummyCalc.Parser.AST

type AddTerm = (La.Operation, Expr)
type MulFactor = (La.Operation, Expr)

{- |
Function `parseProgram` takes a Token list, parses a /program/
and returns a pair consisting of an Either wrapping the
`Program' AST found and the list of Tokens reaminig after the /program/.
An error is denoted by returning the Left Value for the Either
-}
parseProgram :: [Token] -> (Either ParErr Program, [Token])
parseProgram xs =
  case parseStatementList xs of
    (Right s1, ys) -> case parseEOF ys of
                        (Just _, zs) -> (Right (Program s1), zs)
                        (Nothing, _)  -> (Left (MissingEndOfFile xs),ys)
    (Left err@(MissingStatementList _ _ l), _) ->
      (Left $ InvalidProgram err xs (l+1), xs)
    (Left err, _) -> (Left $ InvalidProgram err xs 1, xs)


parseStatementList :: [Token] -> (Either ParErr StatementList, [Token])
parseStatementList xs =
  case parseStatement xs of
    (Left er@(MissingSemiColon _ _), _) ->
      (Left $ MissingStatementList er xs 1, xs)
    (Left er, _) -> (Left $ MissingStatementList er xs 0, xs)
    (Right s1, zs) -> let (ss, st) = parseMoreStatement zs
                      in (Right (s1:ss), st)

parseStatement :: [Token] -> (Either ParErr Statement, [Token])
parseStatement xs =
  case parseAssignExpression xs of
    r@(Right _, _) -> r
    (Left (InvalidAssignExpression _), _) ->
      case parseExpression xs of
        (Right e1, ys) -> case parseEOS ys of
                            (Just _, zs) -> (Right $ Statement e1, zs)
                            (Nothing, _) ->
                              (Left $ MissingSemiColon (Statement e1) ys, ys)
        (Left err, _) -> (Left err, xs)
    (l@(Left _), _) -> (l, xs)
parseAssignExpression :: [Token] -> (Either ParErr Statement, [Token])
parseAssignExpression ((TokVar v):(TokEquals):xs) =
  case parseExpression xs of
    (Right ex, ys) -> case parseEOS ys of
                        (Just _, zs) ->(Right $ Ass v ex, zs)
                        (Nothing, _) ->
                          (Left $ MissingSemiColon (Statement ex) ys, xs)
    (Left l, _) -> (Left l, xs)
parseAssignExpression xs = (Left $ InvalidAssignExpression xs, xs)

parseMoreStatement :: [Token] -> (StatementList, [Token])
parseMoreStatement xs =
  case parseStatement xs of
    (Right s, ys) -> let (statements, st) = parseMoreStatement ys
                     in ((s:statements), st)
    (Left _, _) -> ([], xs)

-- |
-- `parseOEF' returns whether the Token at point is corresponding to End Of
-- File.
parseEOF :: [Token] -> (Maybe EOF, [Token])
parseEOF (TokEof:xs) = (Just EOF, xs)
parseEOF xs = (Nothing, xs)

parseEOS :: [Token] -> (Maybe EOS, [Token])
parseEOS (TokEos:xs) = (Just EOS, xs)
parseEOS xs = (Nothing, xs)
{- |
Function `parseExpression' takes a Token list, parses an
/expression/, and returns a pair consisting of an Either wrapping
the `Expr' AST found and the list of Tokens remaining after the /expression/.
An error is denoted by returning the Left value for the Either

The rule:

@
expression ::= term {addop term}
@

Can be refactored to:

@
expression ::= term moreterms
moreterms  ::= { addterm }
addterm    ::= addop term
@

-}
-- parseExpressionDef
parseExpression :: [Token] -> (Either ParErr Expr, [Token])
parseExpression xs =
  case parseTerm xs of
    (Right t1, ys) ->
      let (terms, zs) = parseMoreTerms ys
      in  (Right (makeBinOpSeq t1 terms), zs)
    (err@(Left _), _) -> (err, xs)

-- Repetition: <moreterms> ::= { <addterm> }
parseMoreTerms :: [Token] -> ([AddTerm], [Token])
parseMoreTerms xs =
  case parseAddTerm xs of
    (Right (op,ex), ys) ->
      let (terms, zs) = parseMoreTerms ys
      in ((op,ex):terms,zs)
    (Left _, _) -> ([], xs)

-- Sequence <addterm> ::= <addop> <term>
parseAddTerm :: [Token] -> (Either ParErr AddTerm, [Token])
parseAddTerm xs'@((TokOperator op):xs)
  | isAddOp op = case parseTerm xs of
                   (Right ex, zs) -> (Right (op,ex), zs)
                   (Left err, _) -> (Left err, xs')
parseAddTerm xs' = (Left $ MissingAddOp xs', xs')

isAddOp :: La.Operation -> Bool
isAddOp La.Summatory = True
isAddOp La.Difference = True
isAddOp _ = False
-- -parseExpressionDef


{- |
Function `parseTerm' takes a Token list, parses a <term>, and returns a pair
consisting of an Either wrapping the Expr AST found and the Tokens remaining
after the <term>. An error is denoted by returning the Left value of the Either.

As with <expression>, the rule:
> <term> ::= <factor> { <mulop> <factor> }

Can be refactored to:

> <term> ::= <factor> <morefactors>
> <morefactors> ::=  { <mulfactor }
> <mulfactor ::= <mulop> <factor>

Each rule will be represented as a function
-}
-- ParseTermsDef
parseTerm :: [Token] -> (Either ParErr Expr, [Token])
parseTerm xs =
  case parseFactor xs of
    (Right f1, ys) ->
     let (factors, zs) = parseMoreFactors ys
     in  (Right (makeBinOpSeq f1 factors), zs)
    (err@(Left _), _) -> (err, xs)


parseMoreFactors :: [Token] -> ([MulFactor], [Token])
parseMoreFactors xs =
  case parseMulFactor xs of
    (Right (op,ex), ys) ->
      let (factors, zs) = parseMoreFactors ys
      in  ((op,ex):factors, zs)
    (Left _, _) -> ([], xs)

parseMulFactor :: [Token] -> (Either ParErr MulFactor, [Token])
parseMulFactor ((TokOperator op):xs)
  | isMulOp op = case parseFactor xs of
                   (Right ex, zs) -> (Right (op, ex), zs)
                   (Left err, _)  -> (Left err, xs)
parseMulFactor xs = (Left $ MissingMulOp xs, xs)

isMulOp :: La.Operation -> Bool
isMulOp La.Multiplication = True
isMulOp La.Division = True
isMulOp _ = False
-- -ParseTermsDef


{- |
Function `parseFactor' takes a Token list, parses a <factor>, and returns a pair
consisting of an Either wrapping the Expr AST and the list of Tokens remaining
after the <factor>. An error is denoted by returning the Left Value for the
Either.

A factor is defined as:
> <factor> ::= <val>
>            | <leftparen> <expression> <rightparen>

This rule can be refactored to:
> <factor> ::= <val> | <nestexpr>
> <nestexpr> ::= <leftparen> <expression> <rightparen>
-}
-- ParseFactorDef
parseFactor :: [Token] -> (Either ParErr Expr, [Token])
parseFactor xs =
  case parseVar xs of
    r@(Right _, _) -> r
    _ ->
      case parseVal xs of
        r@(Right _, _) -> r
        _ ->
          case parseNestExpr xs of
            r@(Right _, _) -> r
            (Left m, ts) -> (Left $ MissingFactor m ts 0, ts)


parseVal :: [Token] -> (Either ParErr Expr, [Token])
parseVal ((TokNumber n):xs) = (Right (Val n), xs)
parseVal ((TokOperator La.Difference):(TokNumber (La.NumValue n)):xs) =
  (Right $ Val $ La.NumValue (-n), xs)
parseVal xs = (Left $ MissingValue xs, xs)

parseVar :: [Token] -> (Either ParErr Expr, [Token])
parseVar ((TokVar x):xs) = (Right (Var x), xs)
parseVar xs = (Left $ NotImplemented "parseVar", xs)

-- | <nestexpr> ::= ( <expr> )
parseNestExpr :: [Token] -> (Either ParErr Expr, [Token])
parseNestExpr xs@(TokLeftParen:ys) =
  case parseExpression ys of
    (ex@(Right _), zs) ->
      case zs of
        (TokRightParen:as) -> (ex,as)
        _                  -> (Left $ MissingRightParen zs, xs)
    (err@(Left _), _) -> (err, xs)
parseNestExpr xs = (Left $ MissingLeftParen xs, xs)
-- -ParseFactorDef
