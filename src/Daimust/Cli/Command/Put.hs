module Daimust.Cli.Command.Put
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            (filtered, (&), (.~), (^.), (^..))
import           Options.Applicative
import           Text.Megaparsec         (parseMaybe)

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (ClientMonad, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose, updateAttendance)
import           Daimust.Data.Attendance


data Args =
  Args
  { _day       :: Text
  , _enter     :: Text
  , _leave     :: Text
  , _noteValue :: Maybe Text
  , _period    :: Maybe Text
  , _verbose   :: Bool
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
    setVerbose _verbose
    let period' = parseMaybe periodP =<< _period
    maybe (pure ()) moveToPeriod period'
    attendances <- listAttendances
    let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
    maybe (pure ()) (update' _enter _leave _noteValue) att

update' :: AttendanceEnter -> AttendanceLeave -> Maybe Text -> Attendance -> ClientMonad ()
update' enter' leave' noteValue' att =
  updateAttendance $ att
  & enter .~ enter'
  & leave .~ leave'
  & noteValue .~ fromMaybe "00" noteValue'

