{-# LANGUAGE TemplateHaskell #-}
module Daimust.Histfile
where


import           ClassyPrelude              hiding (some, (</>))

import           Control.Lens               (to, (^.), (^..), _1)
import           Data.Semigroup             (Max (..), Min (..))
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


try :: IO ()
try = do
  zone <- getCurrentTimeZone
  contents <- readFile . toFilePath =<< histfile
  let lines' = lines $ decodeUtf8 contents
  let hists = catMaybes $ parse' <$> reverse lines'
  let timestamps = hists ^.. traverse . _1 . utcInTZ zone :: [LocalTime]

  traverse_ print . catMaybes $ rangeOfTime <$> groupBy ((==) `on` (^. date)) timestamps

  where
    parse' :: Text -> Maybe (UTCTime, Text)
    parse' = parseMaybe histP

    rangeOfTime :: [LocalTime] -> Maybe (Day, (TimeOfDay, TimeOfDay))
    rangeOfTime [] = Nothing
    rangeOfTime times@(dt : _) =
      case times ^.. traverse . time . to (Min &&& Max) of
        [] -> Nothing
        (x : xs) -> pure $ (dt ^. date,) $ (getMin *** getMax) $ foldl' (<>) x xs



histfile :: IO (Path Abs File)
histfile = maybe dotHistfile parseAbsFile =<< lookupEnv "HISTFILE"
  where
    dotHistfile = do
      home <- getHomeDir
      pure $ home </> $(mkRelFile ".histfile")

histP :: Parsec () Text (UTCTime, Text)
histP = do
  void $ char ':' *> space
  i :: Integer <- decimal
  void $ char ':' *> some digitChar *> char ';'
  command <- takeRest
  pure (posixSecondsToUTCTime $ fromIntegral i, command)
