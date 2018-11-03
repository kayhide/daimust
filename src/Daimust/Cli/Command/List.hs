module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Data.List.Split         (chunksOf)
import           Options.Applicative
import           System.Console.ANSI     (Color (..), ColorIntensity (..),
                                          ConsoleLayer (..), SGR (..), setSGR)
import           Text.Megaparsec         (Parsec, count, parseMaybe)
import           Text.Megaparsec.Char    (digitChar)

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose)
import           Daimust.Data.Attendance (Attendance (..), AttendancePeriod,
                                          formatAttendance)


data Args =
  Args
  { period  :: Maybe Text
  , verbose :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> optional
  ( strOption
    ( long "period" <> metavar "PERIOD" <> help
      "Period to query in a format of YYYYMM"
    ))
  <*> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} = do
  client <- newClient =<< readSettings
  void $ flip runClient client $ do
    setVerbose verbose
    let period' = parseMaybe periodP =<< period
    maybe (pure ()) moveToPeriod period'
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ printAttendance attendances

periodP :: Parsec () Text AttendancePeriod
periodP = do
  year <- pack <$> count 4 digitChar
  month <- pack <$> count 2 digitChar
  pure (year, month)


printAttendance :: Attendance -> IO ()
printAttendance att@Attendance {..} = do
  case (chunksOf 2 . unpack $ drop 1 color) of
    ["ff", "ff", "ff"] -> pure ()
    ["ff", "ff", _]    -> setSGR [SetColor Foreground Vivid Yellow]
    ["ff", _, "ff"]    -> setSGR [SetColor Foreground Vivid Magenta]
    [_, "ff", "ff"]    -> setSGR [SetColor Foreground Vivid Cyan]
    ["ff", _, _]       -> setSGR [SetColor Foreground Vivid Red]
    [_, "ff", _]       -> setSGR [SetColor Foreground Vivid Green]
    [_, _, "ff"]       -> setSGR [SetColor Foreground Vivid Blue]
    _                  -> pure ()
  putStrLn $ formatAttendance att
  setSGR [Reset]
