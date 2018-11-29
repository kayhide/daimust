module Daimust.Cli.Command.Delete
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            (filtered, (&), (.~), (^.), (^..))
import           Options.Applicative

import           Daimust.Cli.Utils       (readSettings, lookupFocus)
import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (ClientMonad, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose, deleteAttendance)
import           Daimust.Data.Attendance


data Args =
  Args
  { _day       :: Text
  , _verbose   :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to delete")
  <*> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} = do
  client <- newClient =<< readSettings
  period' <- lookupFocus
  void $ flip runClient client $ do
    setVerbose _verbose
    maybe (pure ()) moveToPeriod period'
    attendances <- listAttendances
    let att = headMay $ attendances ^.. traverse . filtered ((== _day) . (^. day))
    maybe (pure ()) deleteAttendance att

