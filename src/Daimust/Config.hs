module Daimust.Config where

import           ClassyPrelude

import           Data.Extensible       (nil, shrink)
import           Data.Extensible.Plain (AllOf)

import           Configurable          (RIO, ToConfigs, activate)
import           Daimust.Daim.Config   (DaimConfig)
import           Daimust.Paths.Config  (PathsConfig)
import           Plugin.Logger.Config  (LoggerConfig)


type AppConfig = AllOf (ToConfigs
  '[ LoggerConfig
   , PathsConfig
   , DaimConfig
   ]
  )

activate' :: IO AppConfig
activate' =
  pure nil
  >>= activate @LoggerConfig
  >>= activate @PathsConfig
  >>= activate @DaimConfig
  >>= pure . shrink

runApp :: RIO AppConfig a -> IO a
runApp action = do
  env <- activate'
  runReaderT action env
