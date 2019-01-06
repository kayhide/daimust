module DevMain
where

import           ClassyPrelude

import           System.Environment      (withArgs)

import qualified Daimust.Cli             as Cli
import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client
import           Daimust.Data.Attendance


run :: IO ()
run = do
  runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "prev"]
  -- runCommand ["focus"]
  -- runCommand ["focus", "current"]
  -- replicateM_ 14 $ runCommand ["focus", "next"]
  -- runCommand ["focus"]

  -- runCommand ["put", "26", "1000", "2100"]
  -- runCommand ["delete", "26"]
  runCommand ["list"]

  -- putStrLn "try client"
  -- tryClient

  pure ()

runCommand :: [Text] -> IO ()
runCommand args = do
  putStrLn $ "$ daimust " <> unwords args
  withArgs (unpack <$> args) Cli.run

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
