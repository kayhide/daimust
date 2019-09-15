module Main (main) where

import ClassyPrelude

import qualified Daimust.PathsTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ Daimust.PathsTest.tests
  ]
