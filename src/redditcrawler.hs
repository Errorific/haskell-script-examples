{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Csv                   as Csv
import           Data.Text                  (Text)
import           GHC.Generics
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import           System.Console.CmdArgs

-- reddit crawler
main :: IO ()
main = do
  opts <- cmdArgs options
  r <- WS.withSession getRedditList
  let redditListing = r ^. W.responseBody
  let top10 = map rlidatas . take 10 . Main.children $ datas redditListing
  let csvContents = Csv.encodeDefaultOrderedByName top10
  BSL.writeFile (outputFilename opts) csvContents

data RedditListing = RedditListing
  { kind  :: Text
  , datas :: RedditListingData
  } deriving (Show)

instance FromJSON RedditListing where
 parseJSON (Object v) =
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
 parseJSON (Object v) =
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
instance Csv.ToNamedRecord RedditListingItemData
instance Csv.DefaultOrdered RedditListingItemData

getRedditList :: WS.Session -> IO (W.Response RedditListing)
getRedditList sess = do
  r <- WS.get sess "https://reddit.com/hot.json"
  W.asJSON r

data Options = Options
  { outputFilename :: String
  } deriving (Data, Typeable)

options :: Options
options = Options
  { outputFilename = def
                  &= argPos 0
                  &= typFile
  }
  &= summary "worst reddit app ever"
  &= program "redditcrawler"
