module InterpreterTests where

import Syntax
import Interpreter
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import AuxiliaryFuncs

interpretAndCompareSimpleArithmetic :: TestTree
interpretAndCompareSimpleArithmetic = testGroup "Interpret and compare: programs/simple/arithmetic"
  [ testCase "addition.objb" $ interpretAstAndCompare "programs/simple/arithmetic/addition.objb" []
  , testCase "subtraction.objb" $ interpretAstAndCompare "programs/simple/arithmetic/subtraction.objb" []
  , testCase "multiplication.objb" $ interpretAstAndCompare "programs/simple/arithmetic/multiplication.objb" []
  , testCase "subtraction.objb" $ interpretAstAndCompare "programs/simple/arithmetic/division.objb" []
  ]

interpretAndCompareSimpleLogic :: TestTree
interpretAndCompareSimpleLogic = testGroup "Interpret and compare: programs/simple/logic"
  [ testCase "and.objb" $ interpretAstAndCompare "programs/simple/logic/and.objb" []
  , testCase "or.objb" $ interpretAstAndCompare "programs/simple/logic/or.objb" []
  , testCase "eq.objb" $ interpretAstAndCompare "programs/simple/logic/eq.objb" []
  , testCase "less.objb" $ interpretAstAndCompare "programs/simple/logic/less.objb" []
  ]

interpretAndCompareSimpleCalls :: TestTree
interpretAndCompareSimpleCalls = testGroup "Interpret and compare: programs/simple/branching"
  [ testCase "simplecall1.objb" $ interpretAstAndCompare "programs/simple/calls/simplecall1.objb" []
  , testCase "print.objb" $ interpretAstAndCompare "programs/simple/calls/print.objb" []
  , testCase "nestedcall.objb" $ interpretAstAndCompare "programs/simple/calls/nestedcall.objb" []
  ]

interpretAndCompareSimpleBranching :: TestTree
interpretAndCompareSimpleBranching = testGroup "Interpret and compare: programs/simple/branching"
  [ testCase "if.objb" $ interpretAstAndCompare "programs/simple/branching/if.objb" []
  , testCase "while.objb" $ interpretAstAndCompare "programs/simple/branching/while.objb" []
  ]

interpretAndCompareSimpleBraces :: TestTree
interpretAndCompareSimpleBraces = testGroup "Interpret and compare: programs/simple/braces"
  [ testCase "nestedbraces.objb" $ 
      interpretAstAndCompare "programs/simple/braces/nestedbraces.objb" []
  , testCase "returnbraced.objb" $
      interpretAstAndCompare "programs/simple/braces/returnbraced.objb" []    
  ]

interpretAndCompareSimpleMisc :: TestTree
interpretAndCompareSimpleMisc = testGroup "Interpret and compare: programs/simple/misc"
  [ testCase "comments.objb" $ 
      interpretAstAndCompare "programs/simple/misc/comments.objb" []
  , testCase "emptyStatements.objb" $ 
      interpretAstAndCompare "programs/simple/misc/emptyStatements.objb" []
  , testCase "simplecall1weirdformat.objb" $ 
      interpretAstAndCompare "programs/simple/misc/simplecall1weirdformat.objb" []
  , testCase "manyargs.objb" $ 
      interpretAstAndCompare "programs/simple/misc/manyargs.objb" 
      [(Constant (VNumber 100)), (Constant (VNumber 0)), (Constant (VNumber 0))]
  ]


interpretAndCompareSimpleDumbSemantics :: TestTree
interpretAndCompareSimpleDumbSemantics = 
  testGroup "Interpret and compare: programs/simple/dumbsemantics"
  [ testCase "definereturn.objb" $
      interpretAstAndCompare "programs/simple/dumbsemantics/definereturn.objb" []
  , testCase "exprreturn.objb" $
      interpretAstAndCompare "programs/simple/dumbsemantics/exprreturn.objb" []
  , testCase "mutatereturn.objb" $
      interpretAstAndCompare "programs/simple/dumbsemantics/mutatereturn.objb" []
  , testCase "skipreturn.objb" $
      interpretAstAndCompare "programs/simple/dumbsemantics/skipreturn.objb" []
  , testCase "whilereturn.objb" $
      interpretAstAndCompare "programs/simple/dumbsemantics/whilereturn.objb" []
  ]

interpretErrorAndFail :: TestTree
interpretErrorAndFail = testGroup "Interpret erroneous and fail: programs/error/*"
  [ testCase "badargs/toomany.objb" $ 
      interpretAstAndCompare "programs/error/badargs/toomany.objb" []
  , testCase "badargs/toofew.objb" $ 
      interpretAstAndCompare "programs/error/badargs/toofew.objb" []
  , testCase "missingmain/emptyprogram.objb" $
      interpretAstAndCompare "programs/error/missingmain/emptyprogram.objb" []  
  , testCase "calls/returntype.objb" $
      interpretAstAndCompare "programs/error/calls/returntype.objb" []  
  , testCase "calls/returntype2.objb" $
      interpretAstAndCompare "programs/error/calls/returntype2.objb" []
  , testCase "calls/lostvar.objb" $
      interpretAstAndCompare "programs/error/scopes/lostvar.objb" []
  ]

interpretAndCompareFull :: TestTree
interpretAndCompareFull = testGroup "Interpret and compare: programs/full"
  [ testCase "fib.objb (.out file with (10) as input)" $ 
      interpretAstAndCompare "programs/full/fib.objb" [(Constant (VNumber 10))]
  , testCase "fibbraced.objb (.out file with (10) as input)" $ 
      interpretAstAndCompare "programs/full/fibbraced.objb" [(Constant (VNumber 10))]
  ]