module Main where

import DataType
import Interpreter
import Parser
import System.Environment (getArgs)
import System.IO
import Text.Parsec

initiali = do
  putStrLn "Hello, prolog.hs!"
  args <- getArgs
  premise' <- readFile $ args !! 0
  return $ case parse sentences "prolog" premise' of Right v -> v

repl premise = do
  x <- putStr "prolog.hs> " >> hFlush stdout >> getLine
  print $ case parse literal "prolog" x of
    Left _ -> "error"
    Right v -> show' $ searchCondition premise v
  repl premise

main :: IO ()
main = do
  premise <- initiali
  repl premise
