module Daimust.Cli.Common
  ( Args (..)
  , argsP
  )
where

import ClassyPrelude

import Options.Applicative


data Args =
  Args
  { _verbose  :: Bool
  }
  deriving (Show)

argsP :: Parser Args
argsP =
  Args
  <$> switch (long "verbose" <> short 'v' <> help "Print more")
