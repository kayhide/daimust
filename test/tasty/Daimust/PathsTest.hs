{-# LANGUAGE TemplateHaskell #-}
module Daimust.PathsTest
  (tests)
where

import ClassyPrelude hiding (assert, (</>))

import Configurable (RIO, ToConfigs, activate)
import Daimust.Data.Period (Period)
import Daimust.Paths
import Daimust.Paths.Config (PathsConfig)
import Data.Extensible (nil, shrink)
import Data.Extensible.Plain (AllOf)
import Path
import Path.IO
import Plugin.Logger.Config (LoggerConfig)
import System.Environment (setEnv, unsetEnv)
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck hiding (shrink)

tests :: TestTree
tests = testGroup "Daimust.Paths"
  [ tests_getDaimustDir
  , tests_getCacheFile
  , tests_lookupFocus
  ]


tests_getDaimustDir :: TestTree
tests_getDaimustDir =
  testGroup "getDaimustDir"
  [ testProperty "without DAIMUST_DIR" $
    monadicIO $ do
      x <- run . runApp $ getDaimustDir
      home <- run getHomeDir
      assert $ x == home </> $(mkRelDir ".daimust")

  , after AllFinish "without DAIMUST_DIR" $
    testProperty "with DAIMUST_DIR" $
    monadicIO $ do
      dir' <- run getTempDir
      (expected, x) <- run $ withTempDir dir' "daimust" $ \dir'' -> do
        withEnv "DAIMUST_DIR" (toFilePath dir'') $
          (dir'',) <$> runApp getDaimustDir

      assert $ x == expected
  ]


tests_getCacheFile :: TestTree
tests_getCacheFile =
  after AllFinish "getDaimustDir" $
  testGroup "getCacheFile"
  [ testProperty "without STATE_FILE" $
    monadicIO $ do
      x <- run . runApp $ getCacheFile
      dir <- run . runApp $ getDaimustDir
      assert $ x == dir </> $(mkRelFile "state.bin")

  , after AllFinish "without STATE_FILE" $
    testProperty "with STATE_FILE" $
    \(PrintableString file) -> monadicIO $ do
      pre $ isJust $ parseRelFile file

      x <- run $
        withEnv "STATE_FILE" file $
        runApp getCacheFile

      expected <- run $ (</>) <$> runApp getDaimustDir <*> parseRelFile file
      assert $ x == expected
  ]


tests_lookupFocus :: TestTree
tests_lookupFocus =
  after AllFinish "getDaimustDir" $
  testProperty "lookupFocus" $
  \(period :: Maybe Period) -> monadicIO $ do
    x <- run . runApp $ do
      maybe unfocus focus period
      lookupFocus
    assert $ x == period


-- * Helpers

type AppConfig = AllOf (ToConfigs
  '[ LoggerConfig
   , PathsConfig
   ]
  )

runApp :: RIO AppConfig a -> IO a
runApp action =
  pure nil
  >>= activate @LoggerConfig
  >>= activate @PathsConfig
  >>= pure . shrink
  >>= runReaderT action

withEnv :: String -> String -> IO a -> IO a
withEnv key value action =
  bracket_ (setEnv key value) (unsetEnv key) action
