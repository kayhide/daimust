module Daimust.Cli.Command.Hist
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative

import           Daimust.Config      (AppIO)
import qualified Daimust.Histfile    as Histfile


data Args = Args
  deriving (Show)


argsP :: Parser Args
argsP = pure Args

run :: Args -> AppIO ()
run _ = liftIO $
  traverse_ Histfile.printHistRecord =<< Histfile.readAll
