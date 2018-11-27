module Daimust.Cli
  (run)
where

import           ClassyPrelude

import           Options.Applicative

import qualified Daimust.Cli.Command.Delete as Delete
import qualified Daimust.Cli.Command.Hist   as Hist
import qualified Daimust.Cli.Command.List   as List
import qualified Daimust.Cli.Command.Put    as Put


data CommandArgs where
  List :: List.Args -> CommandArgs
  Put :: Put.Args -> CommandArgs
  Delete :: Delete.Args -> CommandArgs
  Hist :: Hist.Args -> CommandArgs

argsP :: Parser CommandArgs
argsP =
  subparser
  $ command "list" (info (List <$> List.argsP <**> helper) (progDesc "List"))
  <> command "put" (info (Put <$> Put.argsP <**> helper) (progDesc "Put"))
  <> command "delete" (info (Delete <$> Delete.argsP <**> helper) (progDesc "Delete"))
  <> command "hist" (info (Hist <$> Hist.argsP <**> helper) (progDesc "Hist"))


argsWithHelpP :: ParserInfo CommandArgs
argsWithHelpP = info (argsP <**> helper) (fullDesc <> progDesc "Daimust command line tool")

run :: IO ()
run = do
  args <- customExecParser (prefs showHelpOnEmpty) argsWithHelpP
  case args of
    List args'   -> List.run args'
    Put args'    -> Put.run args'
    Delete args' -> Delete.run args'
    Hist args'   -> Hist.run args'
