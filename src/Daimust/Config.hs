module Daimust.Config where

import           ClassyPrelude

import           Data.Extensible       (nil, shrink)
import           Data.Extensible.Plain (AllOf)

import           Configurable          (ToConfigs, activate)
import           Plugin.Logger.Config  (LoggerConfig)


type AppConfig = AllOf (ToConfigs
  '[ LoggerConfig
   ]
  )

activate' :: IO AppConfig
activate' =
  pure nil
  >>= activate @LoggerConfig
  >>= pure . shrink

type RIO env a = ReaderT env IO a

runApp :: RIO AppConfig a -> IO a
runApp action = do
  env <- activate'
  runReaderT action env
