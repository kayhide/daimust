{-# LANGUAGE TemplateHaskell #-}
module Daimust.Histfile
where


import           ClassyPrelude              hiding (some, (</>))

import           Control.Lens               ((%~), (&), (^.), (^..), _1)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Time.Lens
import           Data.Time.LocalTime        (getCurrentTimeZone, utcToLocalTime)
import           Path                       (Abs, File, Path, mkRelFile,
                                             toFilePath, (</>))
import           Path.IO                    (getHomeDir)
import           Text.Megaparsec            (Parsec, parseMaybe, some, takeRest)
import           Text.Megaparsec.Char       (char, digitChar, space)
import           Text.Megaparsec.Char.Lexer (decimal)


try :: IO ()
try = do
  zone <- getCurrentTimeZone
  contents <- readFile . toFilePath =<< histfile
  let lines' = lines $ decodeUtf8 contents
  let hists = catMaybes $ parse' <$> take 100 (reverse lines')
  let locals = hists ^.. traverse . _1 . utcInTZ zone

  traverse_ print' $ zip locals $ snd <$> hists
  -- traverse_ print $ locals ^.. traverse . time

  where
    parse' :: Text -> Maybe (UTCTime, Text)
    parse' = parseMaybe histP

    print' (t, command) = putStrLn $ tshow t <> "  " <> command


histfile :: IO (Path Abs File)
histfile = do
  home <- getHomeDir
  pure $ home </> $(mkRelFile ".histfile")

histP :: Parsec () Text (UTCTime, Text)
histP = do
  void $ char ':' *> space
  i :: Integer <- decimal
  void $ char ':' *> some digitChar *> char ';'
  command <- takeRest
  pure (posixSecondsToUTCTime $ fromIntegral i, command)
