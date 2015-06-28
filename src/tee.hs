{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad
import           System.Console.CmdArgs
import           System.IO

-- tee
main :: IO()
main = do
  opts <- cmdArgs options
  let fileMode = if append opts then AppendMode else WriteMode
  -- Open all the mentioned output files
  outputFileHandles <- mapM (`openFile` fileMode) $ filenames opts
  -- start reading lines from std in
  stdInLines <- liftM lines getContents
  -- for each line, run hsPutStrLn for stdout and all output files
  mapM_ (hsPutStrLn (stdout : outputFileHandles)) stdInLines
  -- close all the open output files so they flush
  mapM_ hClose outputFileHandles

-- print a line to all file handles
hsPutStrLn handles = forM_ handles . flip hPutStrLn

data Options = Options
  { filenames :: [String]
  , append    :: Bool
  } deriving (Data, Typeable)

options :: Options
options = Options
  { filenames = def
             &= args
             &= typ "FILENAME"
  , append = False
          &= help "Append to output file rather than overwrite"
  }
  &= summary "A bad clone of tee"
  &= program "tee"
