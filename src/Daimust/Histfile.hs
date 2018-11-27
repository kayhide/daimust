{-# LANGUAGE TemplateHaskell #-}
module Daimust.Histfile
  ( HistRecord(..)
  , readAll
  , printHistRecord
  )
where


import           ClassyPrelude              hiding (some, (</>))

import           Control.Lens               (to, (^.), (^..), _1, _2)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Semigroup             (Max (..), Min (..), getMax, getMin)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Lens
import           Data.Time.LocalTime        (LocalTime, TimeOfDay,
                                             getCurrentTimeZone)
import           Path                       (Abs, File, Path, mkRelFile,
                                             parseAbsFile, toFilePath, (</>))
import           Path.IO                    (getHomeDir)
import           System.Environment         (lookupEnv)
import           Text.Megaparsec            (Parsec, parseMaybe, some, takeRest)
import           Text.Megaparsec.Char       (char, digitChar, space)
import           Text.Megaparsec.Char.Lexer (decimal)

-- | Data types

data HistRecord = HistRecord
  { day     :: Day
  , startAt :: TimeOfDay
  , endAt   :: TimeOfDay
  }
  deriving (Eq, Show)

-- | Functions

readAll :: IO [HistRecord]
readAll = do
  zone <- getCurrentTimeZone
  contents <- readFile . toFilePath =<< histfile
  let lines' = lines $ decodeUtf8 contents
  let hists = catMaybes $ parseLineMaybe <$> lines'
  let timestamps = hists ^.. traverse . _1 . utcInTZ zone
  pure . fmap toHistRecord $ Map.toList $ foldr pushTime Map.empty timestamps

printHistRecord :: HistRecord -> IO ()
printHistRecord HistRecord {..} =
  putStrLn $ tshow day <> "  " <> tshow startAt <> " - " <> tshow endAt

-- | Internal helpers

type DayMinMaxMap = Map Day (Min TimeOfDay, Max TimeOfDay)

toHistRecord :: (Day, (Min TimeOfDay, Max TimeOfDay)) -> HistRecord
toHistRecord x =
  HistRecord (x ^. _1) (x ^. _2 . _1 . to getMin) (x ^. _2 . _2 . to getMax)

pushTime :: LocalTime -> DayMinMaxMap -> DayMinMaxMap
pushTime t =
  Map.insertWith (<>) (t ^. date) (Min (t ^. time), Max (t ^. time))


histfile :: IO (Path Abs File)
histfile = maybe dotHistfile parseAbsFile =<< lookupEnv "HISTFILE"
  where
    dotHistfile = do
      home <- getHomeDir
      pure $ home </> $(mkRelFile ".histfile")


parseLineMaybe :: Text -> Maybe (UTCTime, Text)
parseLineMaybe = parseMaybe lineP

lineP :: Parsec () Text (UTCTime, Text)
lineP = do
  void $ char ':' *> space
  i :: Integer <- decimal
  void $ char ':' *> some digitChar *> char ';'
  command <- takeRest
  pure (posixSecondsToUTCTime $ fromIntegral i, command)
