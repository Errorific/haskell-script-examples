module Main where

import Data.Foldable       (forM_)
import Options.Applicative (Parser, ParserInfo, argument, auto, execParser,
                            fullDesc, header, help, helper, info, metavar,
                            option, progDesc, short, str, value, (<$>), (<>))

-- head
main :: IO()
main = do
  -- run the parser over the cli arguments
  opts <- execParser optsParserInfo
  -- read the file
  fileContents <- readFile $ filename opts
  -- split the contents of the file into lines
  let fileLines = lines fileContents
  -- take the number of lines specified in the options
  let headLines = take (lineCount opts) fileLines
  -- print the desired lines
  forM_ headLines putStrLn

-- Structure to hold cli arguments
data Options = Options
  { filename  :: String
  , lineCount :: Int
  }

-- Parser for the cli arguments
optsParser :: Parser Options
optsParser = Options
  <$> argument str
    (  metavar "FILENAME"
    <> help "Input filename"
    )
  <*> option auto
    (  short 'n'
    <> long "numlines"
    <> metavar "NUM"
    <> help "Number of lines to read"
    <> value 10
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of head"
  <> header "head - a bad clone of the real head"
  )
