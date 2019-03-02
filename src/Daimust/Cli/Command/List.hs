module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative
import           System.Environment      (setEnv)

import           Daimust.Config          (runApp)
import           Daimust.Daim            (headerTexts, listAttendances,
                                          moveToPeriod, runClient)
import           Daimust.Data.Attendance
import           Daimust.Paths           (lookupFocus)


data Args =
  Args
  { _verbose :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} = do
  when _verbose $
    setEnv "LOGGER_VERBOSE" "true"

  runApp $ do
    period' <- lookupFocus
    runClient $ do
      traverse_ moveToPeriod period'
      headers <- headerTexts
      attendances <- listAttendances

      traverse_ say headers
      traverse_ printAttendance attendances
