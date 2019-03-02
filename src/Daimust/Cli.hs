module Daimust.Cli
  (run)
where

import           ClassyPrelude

import           Options.Applicative
import           System.Environment         (setEnv)

import qualified Daimust.Cli.Command.Delete as Delete
import qualified Daimust.Cli.Command.Focus  as Focus
import qualified Daimust.Cli.Command.Hist   as Hist
import qualified Daimust.Cli.Command.List   as List
import qualified Daimust.Cli.Command.Put    as Put
import qualified Daimust.Cli.Common         as Common
import           Daimust.Config             (AppIO, runApp)


data CommandArgs where
  List :: List.Args -> Common.Args -> CommandArgs
  Put :: Put.Args -> Common.Args -> CommandArgs
  Delete :: Delete.Args -> Common.Args -> CommandArgs
  Hist :: Hist.Args -> Common.Args -> CommandArgs
  Focus :: Focus.Args -> Common.Args -> CommandArgs

argsP :: Parser CommandArgs
argsP =
  subparser
  $ command "list" (info (List <$> List.argsP <*> Common.argsP <**> helper) (progDesc "List"))
  <> command "put" (info (Put <$> Put.argsP <*> Common.argsP <**> helper) (progDesc "Put"))
  <> command "delete" (info (Delete <$> Delete.argsP <*> Common.argsP <**> helper) (progDesc "Delete"))
  <> command "hist" (info (Hist <$> Hist.argsP <*> Common.argsP <**> helper) (progDesc "Hist"))
  <> command "focus" (info (Focus <$> Focus.argsP <*> Common.argsP <**> helper) (progDesc "Focus"))


argsWithHelpP :: ParserInfo CommandArgs
argsWithHelpP = info (argsP <**> helper) (fullDesc <> progDesc "Daimust command line tool")

run :: IO ()
run = do
  args <- customExecParser (prefs showHelpOnEmpty) argsWithHelpP
  case args of
    List args' cmn   -> runApp' cmn $ List.run args'
    Put args' cmn    -> runApp' cmn $ Put.run args'
    Delete args' cmn -> runApp' cmn $ Delete.run args'
    Hist args' cmn   -> runApp' cmn $ Hist.run args'
    Focus args' cmn  -> runApp' cmn $ Focus.run args'


runApp' :: Common.Args -> AppIO a -> IO a
runApp' Common.Args { _verbose } action' = do
  when _verbose $
    setEnv "LOGGER_VERBOSE" "true"
  runApp action'
