module DevMain
where

import           ClassyPrelude

import           Path                    (toFilePath)
import           System.Environment      (withArgs)

import qualified Daimust.Cli             as Cli
import           Daimust.Cli.Utils       (getStateCacheFile, readSettings)
import           Daimust.Client
import qualified Daimust.Crawler         as Crawler
import           Daimust.Data.Attendance


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

  runCommand ["focus", "current", "--verbose"]
  runCommand ["list", "--verbose"]

  -- putStrLn "try client"
  -- tryClient

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

tryClient :: IO ()
tryClient = do
  client <- newClient =<< readSettings
  flip evalClient client $ do
    setVerbose True
    void authenticate
    moveToPeriod $ Period 2018 11
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ printAttendance attendances

    -- let atts = take 2 $ dropWhile (\ Attendance {..} -> date /= "20181005") attendances
    -- let update' att = updateAttendance $ att { enter = "1000", leave = "2200", noteValue = "00" }
    -- let delete' att = deleteAttendance att

    -- liftIO $ putStrLn ""
    -- liftIO $ putStrLn $ "Updating: 20181005 -"
    -- traverse_ update' atts
    -- liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances

    -- liftIO $ putStrLn ""
    -- liftIO $ putStrLn $ "Deleting: 20181005 -"
    -- traverse_ delete' atts
    -- liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances

    pure ()
