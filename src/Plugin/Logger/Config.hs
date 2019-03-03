{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
module Plugin.Logger.Config where

import           ClassyPrelude

import           Control.Lens.TH      (makeFieldsNoPrefix)
import           Control.Monad.Logger

import           Configurable         (Configurable (..), FetchSetting,
                                       fetchSetting)
import           System.Console.ANSI
import           System.Environment   (lookupEnv)


data LoggerConfig

data LoggerSetting = LoggerSetting
  { _stage   :: !Stage
  , _verbose :: !Bool
  , _color   :: !Bool
  }
  deriving (Eq, Show)

data Stage = Development | Production | Test
  deriving (Eq, Show, Read)

instance FetchSetting Stage where
  fetchSetting key def =
    lookupEnv (unpack key)
    >>= \case
      Nothing                         -> pure def
      Just (toLower -> "production")  -> pure Production
      Just (toLower -> "test")        -> pure Test
      Just (toLower -> "development") -> pure Development
      Just x                          -> fail $ "Unknown Stage: " <> x


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
    <*> fetchSetting "LOGGER_COLOR" True

  start (LoggerSetting _stage _verbose _color) _ =
    pure $ LoggerRunning
    $ bool
    (\level' msg' ->
        runStderrLoggingT $
        filterLogger filter' $
        logOtherN level' msg'
    )
    (\level' msg' -> do
        hSetSGR stderr (colorOf level')
        runStderrLoggingT $
          filterLogger filter' $
          logOtherN level' $ msg' <> pack (setSGRCode [Reset])
    )
    _color
    where
      filter' = case (_verbose, _stage) of
        (True, _)           -> \_ _ -> True
        (False, Production) -> \_ level'' -> LevelWarn <= level''
        (False, _)          -> \_ level'' -> LevelInfo <= level''


colorOf :: LogLevel -> [SGR]
colorOf LevelDebug = [SetColor Foreground Dull Green]
colorOf LevelInfo = [SetColor Foreground Dull Blue]
colorOf LevelWarn = [SetColor Foreground Dull Yellow]
colorOf LevelError = [SetColor Foreground Dull Red]
colorOf (LevelOther _) = [SetColor Foreground Dull Magenta]
