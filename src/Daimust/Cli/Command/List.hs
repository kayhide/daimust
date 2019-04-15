module Daimust.Cli.Command.List
  ( Args (..)
  , argsP
  , run
  )
where

import ClassyPrelude

import Options.Applicative

import Daimust.Cli.Utils (printAttendance)
import Daimust.Config (AppIO)
import Daimust.Daim (headerTexts, listAttendances, moveToPeriod, runClient)
import Daimust.Paths (lookupFocus)


data Args = Args
  deriving (Show)


argsP :: Parser Args
argsP = pure Args

run :: Args -> AppIO ()
run _ = do
  period' <- lookupFocus
  runClient $ do
    traverse_ moveToPeriod period'
    headers <- headerTexts
    attendances <- listAttendances

    traverse_ say headers
    traverse_ printAttendance attendances
