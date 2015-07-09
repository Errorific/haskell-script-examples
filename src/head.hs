module Main where

import           Data.Foldable       (forM_)
import           Options.Applicative

-- head
main :: IO()
main = do
  opts <- execParser optsParserInfo
  fileContents <- readFile $ filename opts
  let fileLines = lines fileContents
  let headLines = take (lineCount opts) fileLines
  forM_ headLines putStrLn

data Options = Options
  { filename  :: String
  , lineCount :: Int
  }

optsParser :: Parser Options
optsParser = Options
  <$> argument str
    (  metavar "FILENAME"
    <> help "Input filename")
  <*> option auto
    (  short 'n'
    <> metavar "NUM"
    <> help "Number of lines to read"
    <> value 10)

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of head"
  <> header "head - a bad clone of the real head" )
