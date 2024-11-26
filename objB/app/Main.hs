-- Taken and modified from the Boa assignment.
module Main (main) where

import Syntax
import Interpreter        (execute)
import Parser             (parseString, parseArguments)
import System.Exit        (die)
import System.Environment (getArgs)

run :: Program -> ProcedureInput -> IO ()
run prog input =
  do let (res, out) = execute prog input
     mapM_ putStrLn out
     case res of
       Right v -> putStrLn ("OUT: " ++ show v)
       Left e  -> putStrLn ("*** Runtime error: " ++ show e)

main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file, progArgs] -> do
              s <- readFile file
              case parseArguments progArgs of
                Left err -> putStrLn $ "*** Bad arguments: " ++ show err 
                Right pars -> run (read s) pars
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> putStrLn $ show p
            [file, progArgs] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p ->
                  case parseArguments progArgs of
                    Left err -> putStrLn $ "*** Bad arguments:" ++ show err
                    Right pars -> run p pars
            _ ->
              die "Usage:\n\
                    \  objb -i PROGRAM.objb.ast \"(exp1, exp2 ... expn)\"   (interpret only)\n\
                    \  objb -p PROGRAM.objb                                 (parse only)\n\
                    \  objb PROGRAM.objb        \"(exp1, exp2 ... expn)\"   (parse & interpret)"
