module Main
where

import           ClassyPrelude

import           Options.Applicative

import qualified Daimust.Cli.Command.Delete as Delete
import qualified Daimust.Cli.Command.List   as List
import qualified Daimust.Cli.Command.Put    as Put


data CommandArgs where
  List :: List.Args -> CommandArgs
  Put :: Put.Args -> CommandArgs
  Delete :: Delete.Args -> CommandArgs

argsP :: Parser CommandArgs
argsP =
  subparser
  $ command "list" (info (List <$> List.argsP <**> helper) (progDesc "List"))
  <> command "put" (info (Put <$> Put.argsP <**> helper) (progDesc "Put"))
  <> command "delete" (info (Delete <$> Delete.argsP <**> helper) (progDesc "Delete"))


argsWithHelpP :: ParserInfo CommandArgs
argsWithHelpP = info (argsP <**> helper) (fullDesc <> progDesc "Daimust command line tool")

main :: IO ()
main = do
  args <- customExecParser (prefs showHelpOnEmpty) argsWithHelpP
  case args of
    List args'   -> List.run args'
    Put args'    -> Put.run args'
    Delete args' -> Delete.run args'
