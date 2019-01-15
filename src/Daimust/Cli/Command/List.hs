module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative

import           Daimust.Cli.Utils       (getStateCacheFile, lookupFocus,
                                          readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setCacheFile, setVerbose)
import           Daimust.Data.Attendance


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
  client <- newClient =<< readSettings
  period' <- lookupFocus
  cacheFile' <- getStateCacheFile
  void $ flip runClient client $ do
    setVerbose _verbose
    setCacheFile cacheFile'
    traverse_ moveToPeriod period'
    headers <- headerTexts
    attendances <- listAttendances

    traverse_ putStrLn headers
    traverse_ printAttendance attendances
