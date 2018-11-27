module Daimust.Cli.Command.Hist
  ( Args (..)
  , argsP
  , run
  )
where

import           ClassyPrelude

import           Options.Applicative

import qualified Daimust.Histfile    as Histfile


data Args =
  Args
  { _verbose   :: Bool
  }
  deriving (Show)


argsP :: Parser Args
argsP =
  Args
  <$> switch (long "verbose" <> short 'v' <> help "Print more")

run :: Args -> IO ()
run Args {..} =
  traverse_ Histfile.printHistRecord =<< Histfile.readAll
