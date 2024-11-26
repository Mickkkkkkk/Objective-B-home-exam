module ParserTests where

import Syntax
import Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import AuxiliaryFuncs

parseAndCompareSimpleArithmetic :: TestTree
parseAndCompareSimpleArithmetic = testGroup "Parse and compare: programs/simple/arithmetic"
  [ testCase "addition.objb" $ parseAndCompareAST "programs/simple/arithmetic/addition.objb"
  , testCase "subtraction.objb" $ parseAndCompareAST "programs/simple/arithmetic/subtraction.objb"
  , testCase "multiplication.objb" $ parseAndCompareAST "programs/simple/arithmetic/multiplication.objb"
  , testCase "division.objb" $ parseAndCompareAST "programs/simple/arithmetic/division.objb"
  ]

parseAndCompareSimpleLogic :: TestTree
parseAndCompareSimpleLogic = testGroup "Parse and compare: programs/simple/logic"
  [ testCase "and.objb" $ parseAndCompareAST "programs/simple/logic/and.objb",
    testCase "or.objb" $ parseAndCompareAST "programs/simple/logic/or.objb",
    testCase "eq.objb" $ parseAndCompareAST "programs/simple/logic/eq.objb",
    testCase "less.objb" $ parseAndCompareAST "programs/simple/logic/less.objb"
  ]

parseAndCompareSimpleCalls :: TestTree
parseAndCompareSimpleCalls = testGroup "Parse and compare: programs/simple/calls"
  [ testCase "simplecall1.objb" $ parseAndCompareAST "programs/simple/calls/simplecall1.objb",
    testCase "nestedcall.objb" $ parseAndCompareAST "programs/simple/calls/nestedcall.objb"
  ]

parseAndCompareSimpleBranching :: TestTree
parseAndCompareSimpleBranching = testGroup "Parse and compare: programs/simple/branching"
  [ testCase "while.objb" $ parseAndCompareAST "programs/simple/branching/while.objb"
  , testCase "if.objb" $ parseAndCompareAST "programs/simple/branching/if.objb"
  ]

parseAndCompareSimpleBraces :: TestTree
parseAndCompareSimpleBraces = testGroup "Parse and compare: programs/simple/braces"
  [ testCase "nestedbraces.objb" $ parseAndCompareAST "programs/simple/braces/nestedbraces.objb"
  , testCase "returnbraced.objb" $ parseAndCompareAST "programs/simple/braces/returnbraced.objb"
  ]

parseAndCompareSimpleMisc :: TestTree
parseAndCompareSimpleMisc = testGroup "Parse and compare: programs/simple/misc"
  [ testCase "comments.objb" $ parseAndCompareAST "programs/simple/misc/comments.objb",
    testCase "emptyStatements.objb" $ parseAndCompareAST "programs/simple/misc/emptyStatements.objb",
    testCase "simplecall1weirdformat.objb" $ parseAndCompareAST "programs/simple/misc/simplecall1weirdformat.objb"
  ]

parseAndCompareFull :: TestTree
parseAndCompareFull = testGroup "Parse and compare: programs/full"
  [ testCase "fib.objb" $ parseAndCompareAST "programs/full/fib.objb",
    testCase "fibbraced.objb" $ parseAndCompareAST "programs/full/fibbraced.objb"
  ]

parseErrorFail :: TestTree
parseErrorFail = testGroup "Parse and fail: programs/error/parseerror"
  [ testCase "badwhile.objb" $ parseAndCompareAST "programs/error/parseerror/badwhile.objb"
  , testCase "keywordid.objb" $ parseAndCompareAST "programs/error/parseerror/keywordid.objb"
  ]

parseErrorSucceed  :: TestTree
parseErrorSucceed = testGroup "Parse erroneous and succeed: programs/error/*"
  [ testCase "toomany.objb" $ parseAndCompareAST "programs/error/badargs/toomany.objb"
  , testCase "toofew.objb" $ parseAndCompareAST "programs/error/badargs/toofew.objb"
  ]