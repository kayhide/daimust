{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
module Plugin.Logger.Config where

import           ClassyPrelude

import           Control.Lens.TH      (makeFieldsNoPrefix)
import           Control.Monad.Logger

import           Configurable         (Configurable (..), FetchSetting,
                                       fetchSetting)


data LoggerConfig

data LoggerSetting = LoggerSetting
  { _stage   :: !Stage
  , _verbose :: !Bool
  }
  deriving (Eq, Show)

data Stage = Development | Production | Test
  deriving (Eq, Show, Read)
  deriving FetchSetting

type LogFunc = LogLevel -> Text -> IO ()

data LoggerRunning = LoggerRunning
  { _func :: !LogFunc
  }

instance Show LoggerRunning where
  show (LoggerRunning _) =
    "LoggerRunning "
    <> "{_func = <function>"
    <> "}"

$(makeFieldsNoPrefix ''LoggerSetting)
$(makeFieldsNoPrefix ''LoggerRunning)


instance Configurable LoggerConfig where
  type Setting LoggerConfig = LoggerSetting
  type Running LoggerConfig = LoggerRunning
  type Deps LoggerConfig = '[]

  ready =
    LoggerSetting
    <$> fetchSetting "LOGGER_STAGE" Development
    <*> fetchSetting "LOGGER_VERBOSE" False

  start (LoggerSetting _stage _verbose) _ =
    pure $ LoggerRunning
    (\level' msg' ->
        runStderrLoggingT $
        filterLogger filter' $
        logOtherN level' msg'
    )
    where
      filter' = case (_verbose, _stage) of
        (True, _)           -> \_ _ -> True
        (False, Production) -> \_ level'' -> LevelWarn <= level''
        (False, _)          -> \_ level'' -> LevelInfo <= level''
