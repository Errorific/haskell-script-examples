{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import           System.Console.CmdArgs

-- curl
main :: IO()
main = do
  opts <- cmdArgs options
  responses <- WS.withSession $ \sess ->
    sequence $ map (WS.get sess) (urls opts)
  let bodies = map (\r -> unpack $ r ^. W.responseBody) responses
  putStrLn `mapM_` bodies

data Options = Options
  { urls :: [String]
  } deriving (Data, Typeable)

options :: Options
options = Options
  { urls = def
        &= args
        &= typ "URLS.."
  }
  &= summary "A bad clone of curl"
  &= program "curl"
