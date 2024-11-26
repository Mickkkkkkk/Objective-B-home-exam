module ParseAndInterpretTests where

import Syntax
import Interpreter
import Parser
import AuxiliaryFuncs
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parseAndInterpretSimpleArithmetic :: TestTree
parseAndInterpretSimpleArithmetic = testGroup "Parse, interpret and compare: programs/simple/arithmetic"
  [ testCase "addition.objb" $ 
      parseAndInterpret "programs/simple/arithmetic/addition.objb" []
  , testCase "subtraction.objb" $ 
      parseAndInterpret "programs/simple/arithmetic/subtraction.objb" []
  , testCase "multiplication.objb" $ 
      parseAndInterpret "programs/simple/arithmetic/multiplication.objb" []
  , testCase "division.objb" $ 
      parseAndInterpret "programs/simple/arithmetic/division.objb" []
  ]

parseAndInterpretSimpleLogic :: TestTree
parseAndInterpretSimpleLogic = testGroup "Parse, interpret and compare: programs/simple/logic"
  [ testCase "and.objb" $ parseAndInterpret "programs/simple/logic/and.objb" [],
    testCase "or.objb" $ parseAndInterpret "programs/simple/logic/or.objb" [],
    testCase "eq.objb" $ parseAndInterpret "programs/simple/logic/eq.objb" [],
    testCase "less.objb" $ parseAndInterpret "programs/simple/logic/less.objb" []
  ]

parseAndInterpretSimpleCalls :: TestTree
parseAndInterpretSimpleCalls = testGroup "Parse, interpret and compare: programs/simple/calls"
  [ testCase "simplecall1.objb" $ 
      parseAndInterpret "programs/simple/calls/simplecall1.objb" []
  , testCase "nestedcall.objb" $ 
      parseAndInterpret "programs/simple/calls/nestedcall.objb" []
  ]

parseAndInterpretSimpleBranching :: TestTree
parseAndInterpretSimpleBranching = testGroup "Parse, interpret and compare: programs/simple/branching"
  [ testCase "while.objb" $ 
      parseAndInterpret "programs/simple/branching/while.objb" []
  , testCase "if.objb" $ 
      parseAndInterpret "programs/simple/branching/if.objb" []
  ]

parseAndInterpretSimpleBraces :: TestTree
parseAndInterpretSimpleBraces = testGroup "Parse, interpret and compare: programs/simple/braces"
  [ testCase "nestedbraces.objb" $ 
      parseAndInterpret "programs/simple/braces/nestedbraces.objb" []
  , testCase "returnbraced.objb" $ 
      parseAndInterpret "programs/simple/braces/returnbraced.objb" []
  ]

parseAndInterpretSimpleMisc :: TestTree
parseAndInterpretSimpleMisc = testGroup "Parse, interpret and compare: programs/simple/misc"
  [ testCase "comments.objb" $ 
      parseAndInterpret "programs/simple/misc/comments.objb" []
  , testCase "emptyStatements.objb" $ 
      parseAndInterpret "programs/simple/misc/emptyStatements.objb" []
  , testCase "simplecall1weirdformat.objb" $ 
      parseAndInterpret "programs/simple/misc/simplecall1weirdformat.objb" []
  ]

parseAndInterpretSimpleDumbSemantics :: TestTree
parseAndInterpretSimpleDumbSemantics = 
  testGroup "Parse, interpret and compare: programs/simple/dumbsemantics"
  [ testCase "definereturn.objb" $
      parseAndInterpret "programs/simple/dumbsemantics/definereturn.objb" []
  , testCase "exprreturn.objb" $
      parseAndInterpret "programs/simple/dumbsemantics/exprreturn.objb" []
  , testCase "mutatereturn.objb" $
      parseAndInterpret "programs/simple/dumbsemantics/mutatereturn.objb" []
  , testCase "skipreturn.objb" $
      parseAndInterpret "programs/simple/dumbsemantics/skipreturn.objb" []
  , testCase "whilereturn.objb" $
      parseAndInterpret "programs/simple/dumbsemantics/whilereturn.objb" []
  ]

parseAndInterpretErrorAndFail :: TestTree
parseAndInterpretErrorAndFail = testGroup "Parse and interpret erroneous and fail: programs/error/*"
  [ testCase "badargs/toomany.objb" $ 
      parseAndInterpret "programs/error/badargs/toomany.objb" []
  , testCase "badargs/toofew.objb" $ 
      parseAndInterpret "programs/error/badargs/toofew.objb" []
  , testCase "badargs/badplus1.objb" $ 
      parseAndInterpret "programs/error/badargs/badplus1.objb" []
  , testCase "missingmain/emptyprogram.objb" $
      parseAndInterpret "programs/error/missingmain/emptyprogram.objb" []  
  , testCase "calls/returntype.objb" $
      parseAndInterpret "programs/error/calls/returntype.objb" []  
  , testCase "calls/returntype2.objb" $
      parseAndInterpret "programs/error/calls/returntype2.objb" []
  , testCase "scopes/lostvar.objb" $
      parseAndInterpret "programs/error/scopes/lostvar.objb" []
  , testCase "scopes/redeclaration.objb" $
      parseAndInterpret "programs/error/scopes/redeclaration.objb" []
  ]

parseAndInterpretFull :: TestTree
parseAndInterpretFull = testGroup "Parse, interpret and compare: programs/full"
  [ testCase "fib.objb" $ 
      parseAndInterpret "programs/full/fib.objb" [Constant (VNumber 10)]
  , testCase "fibbraced.objb" $ 
      parseAndInterpret "programs/full/fibbraced.objb" 
      [Operation Plus (Constant (VNumber 5)) (Constant (VNumber 5))]
  , testCase "collatz.objb, input: (5113)" $ 
      parseAndInterpret "programs/full/collatz.objb" [Constant (VNumber 5113)]
  , testCase "gcdsum.objb, input: (28, 8)" $ 
      parseAndInterpret "programs/full/gcdsum.objb" 
      [Constant (VNumber 28), Constant (VNumber 8)]
  ]