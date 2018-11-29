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
  -- withArgs ["focus", "current"] Cli.run
  -- replicateM_ 14 $ withArgs ["focus", "prev"] Cli.run
  -- withArgs ["focus", "current"] Cli.run
  -- replicateM_ 14 $ withArgs ["focus", "next"] Cli.run

  withArgs ["focus", "current"] Cli.run
  replicateM_ 8 $ withArgs ["focus", "prev"] Cli.run
  withArgs ["list"] Cli.run

  -- putStrLn "$ daimust put 26 1000 2100"
  -- withArgs ["put", "26", "1000", "2100"] Cli.run
  -- putStrLn ""
  -- putStrLn "$ daimust delete 26"
  -- withArgs ["delete", "26"] Cli.run
  -- putStrLn ""
  -- putStrLn "$ daimust list"
  -- withArgs ["list"] Cli.run
  -- putStrLn ""
  -- putStrLn "$ daimust list --period 201810"
  -- withArgs ["list", "--period", "201810"] Cli.run
  -- putStrLn ""

  -- putStrLn "try client"
  -- tryClient

  pure ()

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
