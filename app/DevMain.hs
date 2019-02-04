module DevMain
where

import           ClassyPrelude

import           Path                    (toFilePath)
import           System.Environment      (withArgs)

import qualified Daimust.Cli             as Cli
import           Daimust.Cli.Utils       (getStateCacheFile)
import qualified Daimust.Crawler         as Crawler


run :: IO ()
run = do
  -- invalidateCache               -- Invalidate cached state immediately.

  -- runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "prev"]
  -- runCommand ["focus"]
  -- runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "next"]
  -- runCommand ["focus"]

  -- runCommand ["put", "26", "1000", "2100"]
  -- runCommand ["delete", "26"]
  -- runCommand ["list"]

  -- runCommand ["focus", "current", "--verbose"]
  -- runCommand ["list", "--verbose"]

  -- runCommand ["put", "--help"]
  -- runCommand ["put", "31", "1000", "2100", "--note", "21", "--verbose"]
  -- runCommand ["delete", "31"]

  runCommand ["focus", "prev", "--verbose"]
  runCommand ["focus"]
  runCommand ["focus", "none"]
  runCommand ["focus"]
  -- runCommand ["list"]

  pure ()

runCommand :: [Text] -> IO ()
runCommand args = do
  putStrLn $ "$ daimust " <> unwords args
  withArgs (unpack <$> args) Cli.run

invalidateCache :: IO ()
invalidateCache = do
  file <- getStateCacheFile
  Crawler.runCrawler Crawler.getState
    >>= Crawler.dumpState
    >>= writeFile (toFilePath file)
