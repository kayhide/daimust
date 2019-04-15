module Daimust.Paths
  ( getDaimustDir
  , getCacheFile
  , lookupFocus
  , focus
  , unfocus
  )
where

import ClassyPrelude hiding ((</>))

import Control.Lens (view)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Path.IO (doesFileExist, removeFile)

import Configurable (HasConfig, RIO, setting)
import Daimust.Data.Period (Period (..), formatPeriod, parsePeriodMaybe)
import Daimust.Paths.Config

-- * Cli util functions

-- | Daimust directory

getDaimustDir :: (HasConfig env PathsConfig) => RIO env (Path Abs Dir)
getDaimustDir = view $ setting @_ @PathsConfig . daimustDir


-- * Cache

-- | State Cache file

getCacheFile :: (HasConfig env PathsConfig) => RIO env (Path Abs File)
getCacheFile =
  (</>) <$> getDaimustDir <*> (view $ setting @_ @PathsConfig . stateFile)

-- * Focus management functions

-- | Focus file

getFocusFile :: (HasConfig env PathsConfig) => RIO env (Path Abs File)
getFocusFile =
  (</>) <$> getDaimustDir <*> (view $ setting @_ @PathsConfig . focusFile)

withFocusFile :: (HasConfig env PathsConfig) => (Path Abs File -> RIO env r) -> RIO env r
withFocusFile action = do
  action =<< getFocusFile

-- | Try to read current focus

lookupFocus :: (HasConfig env PathsConfig) => RIO env (Maybe Period)
lookupFocus =
  withFocusFile $ \file ->
  doesFileExist file
  >>= bool (pure Nothing) (parsePeriodMaybe <$> readFileUtf8 (toFilePath file))


-- | Set current focus

focus :: (HasConfig env PathsConfig) => Period -> RIO env ()
focus period =
  withFocusFile $ \file ->
  writeFileUtf8 (toFilePath file) $ formatPeriod period

unfocus :: (HasConfig env PathsConfig) => RIO env ()
unfocus =
  withFocusFile $ \file ->
  doesFileExist file
  >>= bool (pure ()) (removeFile file)
