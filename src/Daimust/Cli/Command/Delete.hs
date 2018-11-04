module Daimust.Cli.Command.Delete
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
                                          setVerbose, deleteAttendance)
import           Daimust.Data.Attendance


data Args =
  Args
  { _day       :: Text
  , _period    :: Maybe Text
  , _verbose   :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> argument str (metavar "DAY" <> help "Day to delete")
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
    maybe (pure ()) deleteAttendance att

