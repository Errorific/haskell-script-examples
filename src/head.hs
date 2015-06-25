module Main where

import System.Environment (getArgs)

-- head
main :: IO()
main = do
  args <- getArgs
  let filename = head args
  putStrLn filename
