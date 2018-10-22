module Daimust.Cli.Utils
  ( readSettings
  )
where

import           ClassyPrelude

import           Network.URI             (parseURI)
import           System.Environment

import           Daimust.Client          (Settings (..))

-- * Cli util functions

-- | Read client settings from env vars.

readSettings :: IO Settings
readSettings = do
  Just loginUrl <- parseURI <$> getEnv "DAIM_URL"
  username <- pack <$> getEnv "DAIM_USERNAME"
  password <- pack <$> getEnv "DAIM_PASSWORD"
  pure Settings {..}


