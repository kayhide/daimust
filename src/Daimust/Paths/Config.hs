{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
module Daimust.Paths.Config where

import ClassyPrelude hiding ((</>))

import Control.Lens.TH (makeFieldsNoPrefix)
import Network.URI (URI, parseURI)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, parseAbsDir,
             toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir, getHomeDir, removeFile)

import Configurable (Configurable (..), fetchSetting)
import Plugin.Logger.Config (LoggerConfig)


data PathsConfig

data PathsSetting = PathsSetting
  { _daimustDir :: !(Path Abs Dir)
  , _stateFile  :: !(Path Rel File)
  , _focusFile  :: !(Path Rel File)
  }
  deriving (Eq, Show)

data PathsRunning = PathsRunning
  deriving (Show)

$(makeFieldsNoPrefix ''PathsSetting)
$(makeFieldsNoPrefix ''PathsRunning)


instance Configurable PathsConfig where
  type Setting PathsConfig = PathsSetting
  type Running PathsConfig = PathsRunning
  type Deps PathsConfig = '[LoggerConfig]

  ready = do
    home <- getHomeDir
    let defaultDir = home </> $(mkRelDir ".daimust")
    PathsSetting
      <$> fetchSetting "DAIMUST_DIR" defaultDir
      <*> fetchSetting "STATE_FILE" $(mkRelFile "state.bin")
      <*> fetchSetting "FOCUS_FILE" $(mkRelFile "focus.txt")

  start (PathsSetting daimustDir _ _) _ = do
    ensureDir daimustDir
    pure PathsRunning
