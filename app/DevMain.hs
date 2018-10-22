module DevMain
where

import           ClassyPrelude

import           Formatting

import           Daimust
import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client
import           Daimust.Data.Attendance (formatAttendance)


run :: IO ()
run = do
  client <- newClient =<< readSettings
  flip runClient client $ do
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ (putStrLn . formatAttendance) attendances

    updateAttendance $ Attendance "20181005" "" "" "1000" "2100" "00" ""
    liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances

    deleteAttendance $ Attendance "20181005" "" "" "1000" "2100" "00" ""
    liftIO . traverse_ (putStrLn . formatAttendance) =<< listAttendances



  pure ()
