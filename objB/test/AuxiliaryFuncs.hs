module AuxiliaryFuncs where

import Syntax
import Interpreter
import Parser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parseAndCompareAST :: String -> Assertion
parseAndCompareAST filename =
  do pgm <- readFile filename
     ast <- read <$> readFile (filename ++ ".ast")
     case parseString pgm of
      Left err -> 
        case ast of 
          [] -> return ()
          _ -> assertFailure $ "Program did not parse:\n" ++ (show err)
      Right out -> out @?= ast

compareOutput :: (Either RuntimeError Value, Output) -> String -> Assertion
compareOutput interpreted expected =
  case interpreted of
      (Right result, out) -> (out ++ ["OUT: " ++ (show result)]) @?= lines expected
      (Left err, out) -> (out ++ ["*** Runtime error: " ++ (show err)]) @?= lines expected

interpretAstAndCompare :: String -> ProcedureInput -> Assertion
interpretAstAndCompare filename progArgs =
  do ast <- read <$> (readFile (filename ++ ".ast"))
     out <- readFile $ filename ++ ".out"
     compareOutput (execute ast progArgs) out

parseAndInterpret :: String -> ProcedureInput -> Assertion
parseAndInterpret filename progArgs =
  do pgm <- readFile filename
     out <- readFile $ filename ++ ".out"
     case parseString pgm of
      Left err -> assertFailure $ "Program did not parse:\n" ++ (show err)
      Right ast -> compareOutput (execute ast progArgs) out