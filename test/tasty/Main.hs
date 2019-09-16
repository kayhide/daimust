module Main (main) where

import ClassyPrelude

import qualified Daimust.PathsTest
import Path
import Path.IO
import System.Environment (setEnv, unsetEnv)
import Test.Tasty

main :: IO ()
main = do
  dir' <- getTempDir
  withTempDir dir' "daimust-test" $ \dir'' ->
    bracket_
      (setEnv "HOME" $ toFilePath dir'')
      (unsetEnv "HOME") $
      defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ Daimust.PathsTest.tests
  ]
