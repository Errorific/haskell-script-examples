module Main where

import           System.Environment (getArgs)

-- cat
main :: IO()
main = do
  -- Get a list of args
  args <- getArgs
  -- pull the filename out of the args
  let filename = head args
  -- read the named file
  fileContents <- readFile filename
  -- print the files contents
  putStrLn fileContents
