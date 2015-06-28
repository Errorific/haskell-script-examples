{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Console.CmdArgs 

-- head
main :: IO()
main = do
  args <- cmdArgs options
  fileContents <- readFile $ filename args
  let fileLines = lines fileContents
  let headLines = take (lineCount args) fileLines
  putStrLn `mapM_` headLines

data Options = Options {
  filename :: String,
  lineCount :: Int
} deriving (Data, Typeable)

options :: Options
options = Options
  { filename = def
            &= argPos 0
            &= typ "FILENAME"
  , lineCount = 10
             &= explicit &= name "n"
             &= help "Number of lines to read"
  }
  &= summary "A bad clone of head"
  &= program "head"
