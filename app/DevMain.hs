module DevMain
where

import ClassyPrelude

import Path (toFilePath)
import System.Environment (withArgs)

import qualified Daimust.Cli as Cli
import Daimust.Config (runApp)
import qualified Daimust.Crawler as Crawler
import qualified Daimust.Paths as Paths


run :: IO ()
run = do
  -- invalidateCache               -- Invalidate cached state immediately.

  -- runCommand "put --help"

  -- runCommand "put 12 1000 2100"
  -- runCommand "delete 12"
  -- runCommand "put 2 1000 2100 --holiday-on"
  -- runCommand "delete 2"
  -- runCommand "put 3 --workday-off"
  -- runCommand "delete 3"

  -- runCommand "put --help"
  -- runCommand "put 31 1000 2100 --note 21 --verbose"
  -- runCommand "delete 31"

  -- runCommand "focus current"
  -- runCommand "focus prev"
  -- runCommand "focus none"
  -- runCommand "focus"
  runCommand "list --verbose"

  -- runCommand "hist"

  pure ()

runCommand :: Text -> IO ()
runCommand args = do
  say $ "$ daimust " <> args
  withArgs (unpack <$> words args) Cli.run

invalidateCache :: IO ()
invalidateCache =
  runApp $ do
    file <- Paths.getCacheFile
    Crawler.runCrawler Crawler.getState
      >>= Crawler.dumpState
      >>= writeFile (toFilePath file)
