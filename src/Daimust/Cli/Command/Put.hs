module Daimust.Cli.Command.Put
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative
import           Text.Megaparsec         (parseMaybe)

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (ClientMonad, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose, updateAttendance)
import           Daimust.Data.Attendance (Attendance (..), AttendanceEnter,
                                          AttendanceLeave, periodP)


data Args =
  Args
  { day       :: Text
  , enter     :: Text
  , leave     :: Text
  , noteValue :: Maybe Text
  , period    :: Maybe Text
  , verbose   :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to put")
  <*> argument str (metavar "ENTER" <> help "Enter time HHMM")
  <*> argument str (metavar "LEAVE" <> help "Leave time HHMM")
  <*> optional
  ( strOption
    ( long "note" <> metavar "NOTE" <> help
      "`00` for working day, `20` for holiday (default: 00)"
    ))
  <*> optional
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
    attendances <- listAttendances
    let day' = day
    let att = find (\ Attendance { day } ->  day == day') attendances
    maybe (pure ()) (update' enter leave noteValue) att

update' :: AttendanceEnter -> AttendanceLeave -> Maybe Text -> Attendance -> ClientMonad ()
update' enter' leave' noteValue' Attendance {..} = do
  let enter = enter'
  let leave = leave'
  let noteValue = fromMaybe "00" noteValue'
  updateAttendance $ Attendance {..}

