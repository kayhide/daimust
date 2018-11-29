{-# LANGUAGE TemplateHaskell #-}
module Daimust.Cli.Utils
  ( readSettings
  , getDaimustDir
  , lookupFocus
  , focus
  , unfocus
  )
where

import           ClassyPrelude           hiding ((</>))

import qualified Data.Text.IO            as TIO
import           Network.URI             (parseURI)
import           Path                    (Abs, Dir, File, Path, mkRelDir,
                                          mkRelFile, parseAbsDir, toFilePath,
                                          (</>))
import           Path.IO                 (createDir, doesDirExist,
                                          doesFileExist, ensureDir, getHomeDir,
                                          removeFile)
import           System.Environment      (getEnv, lookupEnv)

import           Daimust.Client          (Settings (..))
import           Daimust.Data.Attendance (Period (..))

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
  withFocusFile $ \file -> do
  exists <- doesFileExist file
  if exists
    then readMay <$> TIO.readFile (toFilePath file)
    else pure Nothing


-- | Set current focus

focus :: Period -> IO ()
focus period = do
  ensureDaimustDir
  file <- getFocusFile
  TIO.writeFile (toFilePath file) $ tshow period

unfocus :: IO ()
unfocus = do
  ensureDaimustDir
  file <- getFocusFile
  exists <- doesFileExist file
  when exists $ removeFile file
