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
import           GHC.Generics               (Generic)
import           Options.Applicative        (Parser, ParserInfo, argument, auto,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, progDesc, str, strOption,
                                             (<*>), (<>))
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hFlush, hSetEcho, stdin, stdout)

-- structure to hold cli arguments
data Options = Options
  { inputFilename :: String
  , dbHost        :: String
  , dbPort        :: Int
  , dbName        :: String
  , dbUser        :: String
  }

-- Parser for cli arguments
optsParser :: Parser Options
optsParser = Options
  <$> argument str
    (  metavar "FILE"
    <> help "input file"
    )
  <*> strOption
    (  long "dbhost"
    <> metavar "HOSTNAME"
    <> help "The hostname of the database"
    )
  <*> option auto
    (  long "dbport"
    <> metavar "NUMBER"
    <> help "The port of the database"
    )
  <*> strOption
    (  long "dbname"
    <> metavar "NAME"
    <> help "The name of the database"
    )
  <*> strOption
    (  long "dbuser"
    <> metavar "USERNAME"
    <> help "The user of the database"
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "A csv importer"
  <> header "dataimporter - a csv importer"
  )

-- data importer
main :: IO ()
main = do
  -- run the options parser over the cli arguments
  opts <- execParser optsParserInfo
  -- read in the users password
  dbPass <- getPassword
  -- read the input files contents
  fileContents <- BSL.readFile $ inputFilename opts
  -- parse the file contents
  let csvContents = decodeCsv fileContents
  -- deal with the csv failing to parse
  case csvContents of Left e -> do
                        -- if it failed, exit like a dirty script
                        putStrLn e
                        exitFailure
                      Right (_, saleRecords) -> do
                        -- connect to the database
                        conn <- makeConnection opts dbPass
                        -- insert the sale records into the db
                        _ <- insertSaleRecords conn saleRecords
                        exitSuccess


-- capture the password with a blind input
getPassword :: IO String
getPassword = do
  -- output some prompt text
  putStr "password: "
  -- flush stdout to make sure the prompt appears
  -- normally only flushes on new lines or buffer being filled
  hFlush stdout
  -- turn off stdin echoing
  hSetEcho stdin False
  -- capture 1 line of input, the password
  dbPass <- getLine
  -- turn stdin echoing back on
  hSetEcho stdin True
  -- print a new line to give the user some feedback
  putStrLn ""
  return dbPass

-- data type representing our csv rows
data SaleRecord = SaleRecord
  { item     :: Text
  , quantity :: Int
  , price    :: Double
  } deriving (Generic)

-- FromNamedRecord lets cassava read a row matching the column titles to
-- record element names
instance Csv.FromNamedRecord SaleRecord

-- decode the csv files contents
decodeCsv :: BSL.ByteString -> Either String (Csv.Header, Vector SaleRecord)
-- decodeByName uses the column headers and FromNamedRecord instance to create
-- the SaleRecords
decodeCsv fileContents = Csv.decodeByName fileContents

-- creates a ConnectInfo from our options and then connects to the db with it
makeConnection :: Options -> String -> IO Psql.Connection
makeConnection opts pass = Psql.connect $ Psql.ConnectInfo
  (dbHost opts)
  (fromIntegral $ dbPort opts)
  (dbUser opts)
  pass
  (dbName opts)

insertSaleRecords :: Psql.Connection -> Vector SaleRecord -> IO (Vector Int64)
insertSaleRecords conn srs = do
  -- run inside of a transaction
  rets <- Psql.withTransaction conn $
          -- runs an insert statement for each SaleRecord
          mapM (insertSaleRecord conn) srs
  return rets

insertSaleRecord :: Psql.Connection -> SaleRecord -> IO Int64
insertSaleRecord conn sr =
  -- execute a raw sql statement with a list of parameters
  Psql.execute conn "insert into sales (item, quantity, price) values (?, ?, ?)"
    [item sr, T.pack . show $ quantity sr, T.pack . show $ price sr]
