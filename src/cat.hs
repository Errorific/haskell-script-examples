module Main where

import Data.Foldable      (forM_)
import System.Environment (getArgs)

-- cat
main :: IO()
main = do
  -- Get a list of args
  args <- getArgs
  -- read the named files
  fileContents <- mapM readFile args
  -- print the files contents
  forM_ fileContents putStrLn
