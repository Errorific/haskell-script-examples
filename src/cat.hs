module Main where

import           System.Environment (getArgs)

-- cat
main :: IO()
main = do
  args <- getArgs
  let filename = head args
  putStrLn filename
