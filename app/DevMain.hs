module DevMain
where

import           ClassyPrelude

import           Formatting
import           System.Environment      (withArgs)

import           Daimust
import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client
import           Daimust.Data.Attendance (formatAttendance)

import qualified Main


run :: IO ()
run = do
  -- putStrLn "$ daimust list"
  -- withArgs ["list"] Main.main
  -- putStrLn ""
  -- putStrLn "$ daimust list --period 201810"
  -- withArgs ["list", "--period", "201810"] Main.main

  putStrLn ""
  putStrLn "try client"
  tryClient

  pure ()

tryClient :: IO ()
tryClient = do
  client <- newClient =<< readSettings
  flip evalClient client $ do
    setVerbose True
    void authenticate
    moveToPeriod ("2018", "10")
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ (putStrLn . formatAttendance) attendances

    let atts = take 2 $ dropWhile (\ Attendance {..} -> date /= "20181005") attendances
    let update' att = updateAttendance $ att { enter = "1000", leave = "2200", noteValue = "00" }
    let delete' att = deleteAttendance att

    liftIO $ putStrLn ""
    liftIO $ putStrLn $ "Updating: 20181005 -"
    traverse_ update' atts
    liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances

    liftIO $ putStrLn ""
    liftIO $ putStrLn $ "Deleting: 20181005 -"
    traverse_ delete' atts
    liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances

    pure ()

