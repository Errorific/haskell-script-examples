module Main where

import           Control.Lens            ((^.))
import           Data.Foldable           (forM_)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO       as TIO (putStrLn)
import qualified Network.Wreq            as W
import qualified Network.Wreq.Session    as WS
import           Options.Applicative     (Parser, ParserInfo, argument,
                                          execParser, fullDesc, header, help,
                                          helper, info, many, metavar, progDesc,
                                          str, (<*>), (<>))

-- curl
main :: IO ()
main = do
  -- run the options parser over the cli arguments
  opts <- execParser optsParserInfo
  -- start up a wreq session
  responses <- WS.withSession $ \sess ->
    -- make a get request for each url
    traverse (WS.get sess) (urls opts)
  -- extract the bodies from each request
  let bodies = map (\r -> decodeUtf8 $ r ^. W.responseBody) responses
  -- print the bodies
  forM_ bodies TIO.putStrLn

-- structure to hold cli arguments
data Options = Options
  { urls :: [String]
  }

-- Parser for cli arguments
optsParser :: Parser Options
optsParser = Options
  <$> many (
    argument str
      (  metavar "URLS"
      <> help "Urls to request"
      )
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A bad clone of curl"
  <> header "curl - a bad clone of the real curl"
  )
