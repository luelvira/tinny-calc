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
        Calc.Equation
              Calc.Equation.Internal
         Calc.CustomErrors
         Calc.Lexer
              Calc.Lexer.AST
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
