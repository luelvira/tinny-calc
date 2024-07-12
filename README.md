# Simple calculator

This application is a toy tool to parse a string and operate with it to return the result of make the operation. The main porpoise of this exercise is to train my knowledge about the languages


# Start the project

In this case, I use cabal as project management for haskell, so to start the configuration, I ran the command `cabal init` and setup the configuration file following the instruction from [Learn haskell by building a blog generator](https://learn-haskell.blog/). This resource also help me with some process that have in common with my idea of the calculator.


## cabal file

The cabal file contains the meta information about the project, so it is important to follow some rules like the names should match.


### Information about the project

The first part contains the information about the project, with the name, description, version, and so on

```cabal
name: lem-calc
version: 0.1.0
synopsis: A small calculator to test haskell
description: This packages provides a small calculator that read an operation from a file and print the result
license: BSD-3-Clause
license-file: LICENSE.txt
author: Lucas Elvira Martín
extra-doc-files:
    README.org
```

When we define export targets, they could share some configuration. To make it, we can define a `common` block that will be named `common-settings` with the compiler and the flags

```cabal
common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall
```


### The library target

This is not necessary, but could be a good practice. I want to build a lexer parser for a dummy calculator. But also, it could be used in another projects, so all the logic will be placed in the `src` folder, and will be called from the `app` folder.

In the `library target` we define:

-   Exposed modules (Modules that need to be compiled and accessible
-   Where is the content of the code, in this case the src folder
-   The dependencies used by the program
-   Other modules that will be empty at this moment

```cabal
library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    Calc
        Calc.Operations
            Calc.Operations.Lexical
        Calc.Equation
              Calc.Equation.Internal
         Calc.CustomErrors
         Calc.Lexer
              Calc.Lexer.AST
              Calc.Lexer.Internal
  -- other-modules:
```


### Executable target

This is the name of the application. In this block, we define the name of the executable and the resources it will need. As we did with the `library target`, we configure it to use the common settings and add the our library as a dependency. Also, we add a new flag to the compilation process and set the folder with the code. In this case the `app` folder.

```cabal
executable lem-calc-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , lem-calc
  ghc-options:
    -O
```


### Test target

I will try to follow as longer as I can a TDD methodology, so this part I thing will be really important, the test block. In this case I use the Spec library that allows me define the test parts. One thing that is very important is to add all the modules which have the test to the build-depends, otherwise, they neither will be compiled or executed.

```cabal
test-suite lem-calc-gen-test
  import: common-settings
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  other-modules:    
    CalcEquationSpec
    OperationsSpec
    CalcSpec
    CalcLexerInternalSpec
  build-depends:
      base
    , hspec
    , hspec-discover
    , raw-strings-qq
    , lem-calc
  ghc-options:
    -O -threaded -rtsopts -with-rtsopts=-N
  build-tool-depends:
    hspec-discover:hspec-discover
```


# The lexer

This is the first part of the parser. This module define the tokens and evaluate if the string could be represent with a custom grammar.

The tokens are defined as:

```haskell
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
```

Some of these tokens will not be used at the moment, but could be used in future versions.

And the grammar is:

```haskell
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
```

A more clear version of the grammar was done with data type

```haskell
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
```


## Tokens

The most reduce piece of the grammar is the factor, which is only a number. The first function will tray to extract the number from the string and returns it and the string remainder

```haskell
-- | Given a string, returns a Value
evalFactor :: String -> Either EvalError (Factor, String)
evalFactor xs = case stringToValue xs of
                  Right (f,s) -> Right (Factor f, s)
                  Left l -> Left l
```

Next, a term is composed by a `Factor` and a list of `(MulOp, factor)`. So, I split the process in two functions.

The first one will be in charge of iter over the string and returns the full list of tuples

```haskell
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
```

With this auxiliary function, I can write the `evalTerm`

```haskell
-- | Given a string, return a Term and the rest of the string not evaluate
evalTerm :: String -> Either EvalError (Term, String)
evalTerm xs = case evalFactor xs of
                Right (value, rest@(_:_)) -> case evalTerm' Nothing rest of
                                             Right (list, restString)
                                               -> Right (Term (value, list), restString)
                                             Left l -> Left l
                Right (v, rest) -> Right (Term (v, []), rest)
                Left l -> Left l
```

Following the same approach, I write the last rule `expression`

```haskell
-- | Given a string, return a Term and the rest of the string not evaluate
evalTerm :: String -> Either EvalError (Term, String)
evalTerm xs = case evalFactor xs of
                Right (value, rest@(_:_)) -> case evalTerm' Nothing rest of
                                             Right (list, restString)
                                               -> Right (Term (value, list), restString)
                                             Left l -> Left l
                Right (v, rest) -> Right (Term (v, []), rest)
                Left l -> Left l
```
