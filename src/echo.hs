module Main where

import Data.List          (intercalate)
import System.Environment (getArgs)

-- echo
main :: IO()
main = do
  -- Get list of args
  args <- getArgs
  -- do something with the args
  let output = intercalate " " args
  -- print some output
  putStrLn output
