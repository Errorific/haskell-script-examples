{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens         ((^.))
import           Control.Monad        (mzero)
import           Data.Aeson           (FromJSON, Object, parseJSON, (.:))
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv             as Csv
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import qualified Network.Wreq         as W
import qualified Network.Wreq.Session as WS
import           Options.Applicative  (Parser, ParserInfo, argument, execParser,
                                       fullDesc, header, help, helper, info,
                                       metavar, progDesc, str, (<*>), (<>))

-- reddit crawler
main :: IO ()
main = do
  -- run the options parser over the cli arguments
  opts <- execParser optsParserInfo
  -- make the call to reddit
  r <- WS.withSession getRedditList
  -- access the data from reddit
  let redditListing = r ^. W.responseBody
  -- get the top 10 listings from the data from reddit
  let top10 = map rlidatas . take 10 . Main.children $ datas redditListing
  -- encode the top 10 listings into a csv format
  let csvContents = Csv.encodeDefaultOrderedByName top10
  -- write the csv formatted data to our output file
  BSL.writeFile (outputFilename opts) csvContents

-- Structures matching the json response from reddit
data RedditListing = RedditListing
  { kind  :: Text
  , datas :: RedditListingData
  } deriving (Show)

-- instance for Aeson to decode JSON into this data structure
instance FromJSON RedditListing where
 parseJSON (Aeson.Object v) =
    RedditListing <$> v .: "kind"
                  <*> v .: "data"
 parseJSON _ = mzero

data RedditListingData = RedditListingData
  { modhash  :: Text
  , children :: [RedditListingItem]
  , after    :: Text
  , before   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON RedditListingData

data RedditListingItem = RedditListingItem
  { rlikind  :: Text
  , rlidatas :: RedditListingItemData
  } deriving (Show)

instance FromJSON RedditListingItem where
 parseJSON (Aeson.Object v) =
    RedditListingItem <$> v .: "kind"
                      <*> v .: "data"
 parseJSON _ = mzero

data RedditListingItemData = RedditListingItemData
  { title     :: Text
  , subreddit :: Text
  , url       :: Text
  , permalink:: Text
  } deriving (Show, Generic)

instance FromJSON RedditListingItemData

-- Instances for turning RedditListingItemData data type into csv rows
-- ToNamedRecord figures out header names from record element names
instance Csv.ToNamedRecord RedditListingItemData
-- DefaultOrdered uses the order of elements in the record for the csv
-- column ordering
instance Csv.DefaultOrdered RedditListingItemData

-- Make a request of reddit decoding the body to a RedditListing
getRedditList :: WS.Session -> IO (W.Response RedditListing)
getRedditList sess = do
  r <- WS.get sess "https://reddit.com/hot.json"
  W.asJSON r

-- structure to hold cli arguments
data Options = Options
  { outputFilename :: String
  }

-- Parser for cli arguments
optsParser :: Parser Options
optsParser = Options
  <$> argument str
    (  metavar "FILENAME"
    <> help "File to output to"
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "The worst reddit client"
  <> header "redditcrawler - a bad reddit client"
  )
