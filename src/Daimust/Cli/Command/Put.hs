module Daimust.Cli.Command.Put
  ( Args (..)
  , argsP
  , run
  )
where

import ClassyPrelude

import Control.Lens (filtered, (&), (?~), (^.), (^?))
import Daimust.Config (AppIO)
import Daimust.Daim (ClientMonad, listAttendances, moveToPeriod, runClient,
                     updateAttendance)
import Daimust.Data.Attendance
import qualified Daimust.Paths as Paths
import Data.Time.Lens (days)
import Options.Applicative
import Text.Megaparsec (parseMaybe)


data AttendityArg where
  DayOn :: AttendanceEnter -> AttendanceLeave -> Attendity -> AttendityArg
  DayOff :: Attendity -> AttendityArg
  deriving (Eq, Show)

data Args =
  Args
  { _day          :: Int
  , _attendityArg :: AttendityArg
  }
  deriving (Show)

dayOnP :: Parser AttendityArg
dayOnP =
  DayOn
  <$> argument (maybeReader (parseMaybe todParser . pack))
  (metavar "ENTER" <> help "Enter time (HHMM|HH:MM)")
  <*> argument (maybeReader (parseMaybe todParser . pack))
  (metavar "LEAVE" <> help "Leave time (HHMM|HH:MM)")
  <*> flag newWorkdayOn newHolidayOn (long "holiday-on" <> help "Specify holiday")

dayOffP :: Parser AttendityArg
dayOffP =
  DayOff
  <$> flag' newWorkdayOff (long "workday-off" <> help "Specify workday off" )

argsP :: Parser Args
argsP =
  Args
  <$> argument auto (metavar "DAY" <> help "Day to put")
  <*> (dayOnP <|> dayOffP)


run :: Args -> AppIO ()
run Args {..} = do
  period' <- Paths.lookupFocus
  runClient $ do
    traverse_ moveToPeriod period'
    attendances <- listAttendances
    let att = attendances ^? traverse . filtered ((== _day) . (^. date . days))
    traverse_ (update' _attendityArg) att

update' :: AttendityArg -> Attendance -> ClientMonad env ()
update' (DayOn enter' leave' attendity') att =
  updateAttendance $ att
  & enter ?~ enter'
  & leave ?~ leave'
  & attendity ?~ attendity'
update' (DayOff attendity') att =
  updateAttendance $ att
  & attendity ?~ attendity'
