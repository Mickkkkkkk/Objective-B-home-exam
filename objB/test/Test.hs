import InterpreterTests
import ParserTests
import ParseAndInterpretTests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
  localOption (mkTimeout 10000000000) $
  testGroup "Test Suite:" $
  [ parserTests
  , interpreterTests
  , errorTests
  , parseAndInterpretTests
  ]

parserTests :: TestTree
parserTests = 
  testGroup "Parser tests:" 
    [ parseAndCompareSimpleArithmetic
    , parseAndCompareSimpleLogic
    , parseAndCompareSimpleCalls
    , parseAndCompareSimpleBranching
    , parseAndCompareSimpleMisc
    , parseAndCompareSimpleBraces
    , parseAndCompareFull
    ]

interpreterTests :: TestTree
interpreterTests =
  testGroup "Interpreter tests:" 
    [ interpretAndCompareSimpleArithmetic
    , interpretAndCompareSimpleLogic
    , interpretAndCompareSimpleCalls
    , interpretAndCompareSimpleBranching
    , interpretAndCompareSimpleBraces
    , interpretAndCompareSimpleMisc
    , interpretAndCompareSimpleDumbSemantics
    , interpretAndCompareFull
    ]

parseAndInterpretTests :: TestTree
parseAndInterpretTests = 
  testGroup "Parse and interpret tests:"
    [ parseAndInterpretSimpleArithmetic
    , parseAndInterpretSimpleLogic
    , parseAndInterpretSimpleCalls
    , parseAndInterpretSimpleBranching
    , parseAndInterpretSimpleBraces
    , parseAndInterpretSimpleMisc
    , parseAndInterpretSimpleDumbSemantics
    , parseAndInterpretFull
    ]

errorTests :: TestTree
errorTests = 
  testGroup "Test faulty programs:"
  [ parseErrorFail,
    parseErrorSucceed,
    interpretErrorAndFail,
    parseAndInterpretErrorAndFail
  ]