module Main
where

import           ClassyPrelude

import           Options.Applicative

import qualified Daimust.Cli.Command.List as List


data CommandArgs where
  List :: List.Args -> CommandArgs

argsP :: Parser CommandArgs
argsP =
  subparser
  (command "list" (info (List <$> List.argsP <**> helper) (progDesc "List")))


argsWithHelpP :: ParserInfo CommandArgs
argsWithHelpP = info (argsP <**> helper) (fullDesc <> progDesc "Daimust command line tool")

main :: IO ()
main = do
  args <- customExecParser (prefs showHelpOnEmpty) argsWithHelpP
  case args of
    List args' -> List.run args'
