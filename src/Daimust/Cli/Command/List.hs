module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Control.Lens            ((^.))
import           Data.List.Split         (chunksOf)
import           Options.Applicative
import           System.Console.ANSI     (Color (..), ColorIntensity (..),
                                          ConsoleLayer (..), SGR (..), setSGR)
import           Text.Megaparsec         (parseMaybe)

import           Daimust.Cli.Utils       (readSettings)
import           Daimust.Client          (headerTexts, listAttendances,
                                          moveToPeriod, newClient, runClient,
                                          setVerbose)
import           Daimust.Data.Attendance


data Args =
  Args
  { _period  :: Maybe Text
  , _verbose :: Bool
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
    setVerbose _verbose
    let period' = parseMaybe periodP =<< _period
    maybe (pure ()) moveToPeriod period'
    headers <- headerTexts
    attendances <- listAttendances
    liftIO $ do
      traverse_ putStrLn headers
      traverse_ print' attendances

print' :: Attendance -> IO ()
print' att = do
  case (chunksOf 2 . unpack $ drop 1 $ att ^. color) of
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
