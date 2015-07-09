module Main where

import           Control.Monad       (liftM)
import           Data.Foldable       (forM_)
import           Options.Applicative
import           System.IO

-- tee
main :: IO()
main = do
  opts <- execParser optsParserInfo
  let fileMode = if append opts then AppendMode else WriteMode
  -- Open all the mentioned output files
  outputFileHandles <- mapM (`openFile` fileMode) $ filenames opts
  -- start reading lines from std in
  stdInLines <- liftM lines getContents
  -- for each line, run hsPutStrLn for stdout and all output files
  forM_ stdInLines $ hsPutStrLn (stdout : outputFileHandles)
  -- close all the open output files so they flush
  mapM_ hClose outputFileHandles

-- print a line to all file handles
hsPutStrLn handles = forM_ handles . flip hPutStrLn

data Options = Options
  { filenames :: [String]
  , append    :: Bool
  } deriving (Show)

optsParser :: Parser Options
optsParser = Options
  <$> many (argument str
    (  metavar "FILENAME..."
    <> help "Output files"))
  <*> switch
    (  long "append"
    <> short 'a'
    <> help "Append to output file rather than overwrite")

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of tee"
  <> header "tee - a bad clone of the real tee")
