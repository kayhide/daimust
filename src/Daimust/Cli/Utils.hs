{-# LANGUAGE TemplateHaskell #-}
module Daimust.Cli.Utils
  ( readSettings
  , getDaimustDir
  , getStateCacheFile
  , lookupFocus
  , focus
  , unfocus
  )
where

import           ClassyPrelude       hiding ((</>))

import           Network.URI         (parseURI)
import           Path                (Abs, Dir, File, Path, mkRelDir, mkRelFile,
                                      parseAbsDir, toFilePath, (</>))
import           Path.IO             (doesFileExist, ensureDir, getHomeDir,
                                      removeFile)
import           System.Environment  (getEnv, lookupEnv)

import           Daimust.Client      (Settings (..))
import           Daimust.Data.Period (Period (..), formatPeriod,
                                      parsePeriodMaybe)

-- * Cli util functions

-- | Read client settings from env vars.

readSettings :: IO Settings
readSettings = do
  Just loginUrl <- parseURI <$> getEnv "DAIM_URL"
  username <- pack <$> getEnv "DAIM_USERNAME"
  password <- pack <$> getEnv "DAIM_PASSWORD"
  pure Settings {..}


-- | Daimust directory

getDaimustDir :: IO (Path Abs Dir)
getDaimustDir = maybe defaultDir parseAbsDir =<< lookupEnv "DAIMUST_DIR"
  where
    defaultDir = do
      home <- getHomeDir
      pure $ home </> $(mkRelDir ".daimust")

-- | Create daimust directory if not present yet

ensureDaimustDir :: IO ()
ensureDaimustDir = ensureDir =<< getDaimustDir


-- * Cache

-- | State Cache file

getStateCacheFile :: IO (Path Abs File)
getStateCacheFile = do
  dir <- getDaimustDir
  pure $ dir </> $(mkRelFile "state.bin")

-- * Focus management functions

-- | Focus file

getFocusFile :: IO (Path Abs File)
getFocusFile = do
  dir <- getDaimustDir
  pure $ dir </> $(mkRelFile "focus.txt")

withFocusFile :: (Path Abs File -> IO r) -> IO r
withFocusFile action = do
  ensureDaimustDir
  action =<< getFocusFile

-- | Try to read current focus

lookupFocus :: IO (Maybe Period)
lookupFocus =
  withFocusFile $ \file ->
  doesFileExist file
  >>= bool (pure Nothing) (parsePeriodMaybe <$> readFileUtf8 (toFilePath file))


-- | Set current focus

focus :: Period -> IO ()
focus period =
  withFocusFile $ \file ->
  writeFileUtf8 (toFilePath file) $ formatPeriod period

unfocus :: IO ()
unfocus =
  withFocusFile $ \file ->
  doesFileExist file
  >>= bool (pure ()) (removeFile file)
