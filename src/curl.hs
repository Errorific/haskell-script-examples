{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Foldable              (forM_)
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import           System.Console.CmdArgs

-- curl
main :: IO ()
main = do
  opts <- cmdArgs options
  responses <- WS.withSession $ \sess ->
    traverse (WS.get sess) (urls opts)
  let bodies = map (\r -> unpack $ r ^. W.responseBody) responses
  forM_ bodies putStrLn

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
