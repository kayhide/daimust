module Daimust.Cli.Command.Delete
  ( Args (..)
  , argsP
  , run
  )
where

import ClassyPrelude

import Control.Lens (filtered, (^.), (^..))
import Options.Applicative

import Daimust.Config (AppIO)
import Daimust.Daim (deleteAttendance, listAttendances, moveToPeriod, runClient)
import Daimust.Data.Attendance
import Daimust.Paths (lookupFocus)


data Args =
  Args
  { _day     :: Text
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to delete")

run :: Args -> AppIO ()
run Args {..} = do
  period' <- lookupFocus
  runClient $ do
    maybe (pure ()) moveToPeriod period'
    attendances <- listAttendances
    let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
    maybe (pure ()) deleteAttendance att

