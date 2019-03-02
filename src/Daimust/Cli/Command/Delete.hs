module Daimust.Cli.Command.Delete
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            (filtered, (^.), (^..))
import           Options.Applicative
import           System.Environment      (setEnv)

import           Daimust.Config          (runApp)
import           Daimust.Daim            (deleteAttendance, listAttendances,
                                          moveToPeriod, runClient)
import           Daimust.Data.Attendance
import           Daimust.Paths           (lookupFocus)


data Args =
  Args
  { _day     :: Text
  , _verbose :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to delete")
  <*> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} = do
  when _verbose $
    setEnv "LOGGER_VERBOSE" "true"

  runApp $ do
    period' <- lookupFocus
    runClient $ do
      maybe (pure ()) moveToPeriod period'
      attendances <- listAttendances
      let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
      maybe (pure ()) deleteAttendance att

