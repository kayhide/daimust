{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
module Daimust.Daim.Config where

import           ClassyPrelude

import           Control.Lens.TH      (makeFieldsNoPrefix)
import           Network.URI          (URI, parseURI)

import           Configurable         (Configurable (..), fetchSetting)
import           Plugin.Logger.Config (LoggerConfig)


data DaimConfig

data DaimSetting = DaimSetting
  { _url      :: !URI
  , _username :: !Text
  , _password :: !Text
  }
  deriving (Eq, Show)

data DaimRunning = DaimRunning
  deriving (Show)

$(makeFieldsNoPrefix ''DaimSetting)
$(makeFieldsNoPrefix ''DaimRunning)


instance Configurable DaimConfig where
  type Setting DaimConfig = DaimSetting
  type Running DaimConfig = DaimRunning
  type Deps DaimConfig = '[LoggerConfig]

  ready = do
    let Just defaultUrl = parseURI "https://pay.kizunajapan.co.jp/itr/n/MainLogin.php"
    DaimSetting
      <$> fetchSetting "DAIM_URL" defaultUrl
      <*> fetchSetting "DAIM_USERNAME" ""
      <*> fetchSetting "DAIM_PASSWORD" ""

  start _ _ = pure DaimRunning
