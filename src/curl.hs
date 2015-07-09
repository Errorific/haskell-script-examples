module Main where

import           Control.Lens               ((^.))
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Foldable              (forM_)
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import           Options.Applicative

-- curl
main :: IO ()
main = do
  opts <- execParser optsParserInfo
  responses <- WS.withSession $ \sess ->
    traverse (WS.get sess) (urls opts)
  let bodies = map (\r -> unpack $ r ^. W.responseBody) responses
  forM_ bodies putStrLn

data Options = Options
  { urls :: [String]
  }

optsParser :: Parser Options
optsParser = Options
  <$> many (
    argument str
      (  metavar "URLS"
      <> help "Urls to request"))

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of curl"
  <> header "curl - a bad clone of the real curl")
