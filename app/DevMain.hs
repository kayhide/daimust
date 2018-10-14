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
  client <-
    readSettings
    >>= newClient
    >>= authenticate
  traverse_ putStrLn $ headerTexts client
  traverse_ (putStrLn . formatAttendence) $ listAttendances client

  pure ()


formatAttendence :: Attendance -> Text
formatAttendence Attendance {..} =
  sformat
  ( (left 4 ' ' %. stext) %
    (left 4 ' ' %. stext) %
    " " % (center 15 ' ' %. stext) %
    " " % stext
  ) day dow hours note


readSettings :: IO Settings
readSettings = do
  Just loginUrl <- parseURI <$> getEnv "DAIM_URL"
  username <- pack <$> getEnv "DAIM_USERNAME"
  password <- pack <$> getEnv "DAIM_PASSWORD"
  pure Settings {..}
