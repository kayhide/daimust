module Daimust.Cli.Command.Put
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            (filtered, (&), (.~), (^.), (^..))
import           Options.Applicative

import           Daimust.Config          (AppIO)
import           Daimust.Daim            (ClientMonad, listAttendances,
                                          moveToPeriod, runClient,
                                          updateAttendance)
import           Daimust.Data.Attendance
import qualified Daimust.Paths           as Paths


data Args =
  Args
  { _day       :: Text
  , _enter     :: AttendanceEnter
  , _leave     :: AttendanceLeave
  , _noteValue :: Text
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to put")
  <*> argument str (metavar "ENTER" <> help "Enter time HHMM")
  <*> argument str (metavar "LEAVE" <> help "Leave time HHMM")
  <*> strOption ( long "note" <> metavar "NOTE" <> value "00" <> showDefault <> help
                  "`00` for working day, `20` for holiday"
                )

run :: Args -> AppIO ()
run Args {..} = do
  period' <- Paths.lookupFocus
  runClient $ do
    maybe (pure ()) moveToPeriod period'
    attendances <- listAttendances
    let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
    maybe (pure ()) (update' _enter _leave _noteValue) att

update' :: AttendanceEnter -> AttendanceLeave -> Text -> Attendance -> ClientMonad env ()
update' enter' leave' noteValue' att =
  updateAttendance $ att
  & enter .~ enter'
  & leave .~ leave'
  & noteValue .~ noteValue'

