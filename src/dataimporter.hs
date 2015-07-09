{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Csv                   as Csv
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Database.PostgreSQL.Simple as Psql
import           GHC.Generics
import           Options.Applicative
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hFlush, hSetEcho, stdin, stdout)

-- data importer
main :: IO ()
main = do
  opts <- execParser optsParserInfo
  dbPass <- getPassword
  fileContents <- BSL.readFile $ inputFilename opts
  let csvContents = Csv.decodeByName fileContents :: Either String (Csv.Header, Vector SaleRecord)
  case csvContents of Left e -> do
                        putStrLn e
                        exitFailure
                      Right (_, saleRecords) -> do
                        conn <- makeConnection opts dbPass
                        _ <- insertSaleRecords conn saleRecords
                        exitSuccess

getPassword :: IO String
getPassword = do
  _ <- putStr "password: "
  _ <- hFlush stdout
  _ <- hSetEcho stdin False
  dbPass <- getLine
  _ <- hSetEcho stdin True
  _ <- putStrLn ""
  return dbPass

data SaleRecord = SaleRecord
  { item     :: Text
  , quantity :: Int
  , price    :: Double
  } deriving (Show, Generic)

instance Csv.ToNamedRecord SaleRecord
instance Csv.FromNamedRecord SaleRecord
instance Csv.DefaultOrdered SaleRecord

makeConnection :: Options -> String -> IO Psql.Connection
makeConnection opts pass = Psql.connect $ Psql.ConnectInfo
  (dbHost opts)
  (fromIntegral $ dbPort opts)
  (dbUser opts)
  pass
  (dbName opts)

insertSaleRecords :: Psql.Connection -> Vector SaleRecord -> IO (Vector Int64)
insertSaleRecords conn srs = do
  _ <- Psql.begin conn
  rets <- mapM (insertSaleRecord conn) srs
  _ <- Psql.commit conn
  return rets

insertSaleRecord :: Psql.Connection -> SaleRecord -> IO Int64
insertSaleRecord conn sr =
  Psql.execute conn "insert into sales (item, quantity, price) values (?, ?, ?)"
    [item sr, T.pack . show $ quantity sr, T.pack . show $ price sr]

data Options = Options
  { inputFilename :: String
  , dbHost        :: String
  , dbPort        :: Int
  , dbName        :: String
  , dbUser        :: String
  } deriving (Show)

optsParser :: Parser Options
optsParser = Options
  <$> argument str
    (  metavar "FILE"
    <> help "input file"
    )
  <*> strOption
    (  long "dbhost"
    <> help "The hostname of the database"
    )
  <*> option auto
    (  long "dbport"
    <> help "The port of the database"
    )
  <*> option auto
    (  long "dbname"
    <> help "The name of the database"
    )
  <*> option auto
    (  long "dbuser"
    <> help "The user of the database"
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A csv importer"
  <> header "dataimporter - a csv importer"
  )
