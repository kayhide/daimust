module DevMain
where

import           ClassyPrelude

import           Formatting
import           Network.URI        (parseURI)
import           System.Environment

import           Daimust
import           Daimust.Client


run :: IO ()
run = do
  client <- newClient =<< readSettings
  flip runClient client $ do
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn $ headers
      traverse_ (putStrLn . formatAttendence) $ attendances

    updateAttendance $ Attendance "20181005" "" "" "1000" "2100" "00" ""
    liftIO . traverse_ (putStrLn . formatAttendence) =<< listAttendances

    deleteAttendance $ Attendance "20181005" "" "" "1000" "2100" "00" ""
    liftIO . traverse_ (putStrLn . formatAttendence) =<< listAttendances



  pure ()


formatAttendence :: Attendance -> Text
formatAttendence Attendance {..} =
  sformat
  ( (left 4 ' ' %. stext) %
    (left 4 ' ' %. stext) %
    " " % (center 15 ' ' %. (stext % " - " % stext)) %
    " " % stext %
    (later (\s -> bool (bprint (" (" % stext % ")") s) "" $ null s))
  ) day dow enter leave noteLabel noteValue


readSettings :: IO Settings
readSettings = do
  Just loginUrl <- parseURI <$> getEnv "DAIM_URL"
  username <- pack <$> getEnv "DAIM_USERNAME"
  password <- pack <$> getEnv "DAIM_PASSWORD"
  pure Settings {..}
