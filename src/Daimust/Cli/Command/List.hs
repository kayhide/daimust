module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          newClient, runClient, setVerbose)
import           Daimust.Data.Attendance (formatAttendance)


data Args =
  Args
  { month   :: Maybe Text
  , verbose :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> optional
  ( strOption
    ( long "month" <> short 'm' <> metavar "MONTH" <> help
      "Month to query in a format of YYYYMM"
    ))
  <*> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args { verbose } = do
  client <- newClient =<< readSettings
  void $ flip runClient client $ do
    setVerbose verbose
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ (putStrLn . formatAttendance) attendances
