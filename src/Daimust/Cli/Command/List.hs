module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative
import           Text.Megaparsec         (Parsec, count, parseMaybe)
import           Text.Megaparsec.Char    (digitChar)

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose)
import           Daimust.Data.Attendance (AttendancePeriod, formatAttendance)


data Args =
  Args
  { period  :: Maybe Text
  , verbose :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> optional
  ( strOption
    ( long "period" <> metavar "PERIOD" <> help
      "Period to query in a format of YYYYMM"
    ))
  <*> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} = do
  client <- newClient =<< readSettings
  void $ flip runClient client $ do
    setVerbose verbose
    let period' = parseMaybe periodP =<< period
    maybe (pure ()) moveToPeriod period'
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ (putStrLn . formatAttendance) attendances

periodP :: Parsec () Text AttendancePeriod
periodP = do
  year <- pack <$> count 4 digitChar
  month <- pack <$> count 2 digitChar
  pure (year, month)
