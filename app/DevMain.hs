module DevMain
where

import           ClassyPrelude

import           Path               (toFilePath)
import           System.Environment (withArgs)

import qualified Daimust.Cli        as Cli
import           Daimust.Config     (runApp)
import qualified Daimust.Crawler    as Crawler
import qualified Daimust.Paths      as Paths


run :: IO ()
run = do
  -- invalidateCache               -- Invalidate cached state immediately.

  -- runCommand ["--help"]
  -- runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "prev"]
  -- runCommand ["focus"]
  -- runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "next"]
  -- runCommand ["focus"]

  -- runCommand ["put", "26", "1000", "2100"]
  -- runCommand ["list"]
  -- runCommand ["delete", "26"]
  -- runCommand ["list"]

  -- runCommand ["focus", "current", "--verbose"]
  -- runCommand ["list", "--verbose"]

  -- runCommand ["put", "--help"]
  -- runCommand ["put", "31", "1000", "2100", "--note", "21", "--verbose"]
  -- runCommand ["delete", "31"]

  -- runCommand ["focus", "prev", "--verbose"]
  -- runCommand ["focus"]
  -- runCommand ["focus", "none"]
  -- runCommand ["focus"]
  runCommand ["list", "--verbose"]

  pure ()

runCommand :: [Text] -> IO ()
runCommand args = do
  say $ "$ daimust " <> unwords args
  withArgs (unpack <$> args) Cli.run

invalidateCache :: IO ()
invalidateCache =
  runApp $ do
    file <- Paths.getCacheFile
    Crawler.runCrawler Crawler.getState
      >>= Crawler.dumpState
      >>= writeFile (toFilePath file)
