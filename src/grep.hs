module Main where

import Data.List          (isInfixOf)
import System.Environment (getArgs)

-- grep
main :: IO()
main = do
  args <- getArgs
  
  --Get the search pattern from the first argument
  let pattern = head args
  
  --Read all file names on command line
  fileContents <- mapM readFile $ tail args

  let lineContents = concat $ map lines fileContents
  let selection = filter (isInfixOf pattern) lineContents

  mapM_ putStrLn selection
