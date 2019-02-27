module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative
import           System.Environment      (setEnv)

import           Configurable            (HasConfig)
import           Daimust.Cli.Utils       (getStateCacheFile, lookupFocus,
                                          readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setCacheFile, setVerbose)
import           Daimust.Config          (RIO)
import           Daimust.Config          (activate', runApp)
import           Daimust.Data.Attendance
import qualified Plugin.Logger           as Logger


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

  runApp $ liftIO $ do
    client <- newClient =<< readSettings
    period' <- lookupFocus
    cacheFile' <- getStateCacheFile
    void $ flip runClient client $ do
      setVerbose _verbose
      setCacheFile cacheFile'
      traverse_ moveToPeriod period'
      headers <- headerTexts
      attendances <- listAttendances

      traverse_ say headers
      traverse_ printAttendance attendances
