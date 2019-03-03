module Daimust.Cli.Command.Put
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            (filtered, (&), (.~), (?~), (^.),
                                          (^..))
import           Options.Applicative

import           Daimust.Config          (AppIO)
import           Daimust.Daim            (ClientMonad, listAttendances,
                                          moveToPeriod, runClient,
                                          updateAttendance)
import           Daimust.Data.Attendance
import qualified Daimust.Paths           as Paths


data AttendityArg =
  DayOn AttendanceEnter AttendanceLeave Attendity
  | DayOff Attendity
  deriving (Eq, Show)

data Args =
  Args
  { _day          :: Text
  , _attendityArg :: AttendityArg
  }
  deriving (Show)

dayOnP :: Parser AttendityArg
dayOnP =
  DayOn
  <$> argument str (metavar "ENTER" <> help "Enter time HHMM")
  <*> argument str (metavar "LEAVE" <> help "Leave time HHMM")
  <*> flag newWorkdayOn newHolidayOn (long "holiday-on" <> help "Specify holiday")

dayOffP :: Parser AttendityArg
dayOffP =
  DayOff
  <$> flag' newWorkdayOff (long "workday-off" <> help "Specify workday off" )

argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to put")
  <*> (dayOnP <|> dayOffP)


run :: Args -> AppIO ()
run Args {..} = do
  period' <- Paths.lookupFocus
  runClient $ do
    maybe (pure ()) moveToPeriod period'
    attendances <- listAttendances
    let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
    maybe (pure ()) (update' _attendityArg) att

update' :: AttendityArg -> Attendance -> ClientMonad env ()
update' (DayOn enter' leave' attendity') att =
  updateAttendance $ att
  & enter .~ enter'
  & leave .~ leave'
  & attendity ?~ attendity'
update' (DayOff attendity') att =
  updateAttendance $ att
  & attendity ?~ attendity'
