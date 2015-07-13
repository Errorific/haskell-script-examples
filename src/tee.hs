module Main where

import Control.Monad       (liftM)
import Data.Foldable       (forM_)
import Options.Applicative (Parser, ParserInfo, argument, execParser, fullDesc,
                            header, help, helper, info, long, many, metavar,
                            progDesc, short, str, switch, (<*>), (<>))
import System.IO           (Handle, IOMode (AppendMode), IOMode (WriteMode),
                            hClose, hPutStrLn, openFile, stdout)

-- tee
main :: IO ()
main = do
  -- run the parser over the cli argumentts
  opts <- execParser optsParserInfo
  -- Pick file mode based on option
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
hsPutStrLn :: [Handle] -> String -> IO ()
hsPutStrLn handles = forM_ handles . flip hPutStrLn

-- structure to hold cli arguments
data Options = Options
  { filenames :: [String]
  , append    :: Bool
  } deriving (Show)

-- Parser for cli arguments
optsParser :: Parser Options
optsParser = Options
  <$> many (
    argument str
      (  metavar "FILENAMES"
      <> help "Output files"))
  <*> switch
    (  long "append"
    <> short 'a'
    <> help "Append to output file rather than overwrite")

-- Adding program help text to the parser
optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of tee"
  <> header "tee - a bad clone of the real tee")
