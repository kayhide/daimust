module DevMain
where

import           ClassyPrelude        hiding (many, some)

import           Control.Lens         (at, folded, folding, to, universe, (&),
                                       (.~), (?~), (^.), (^..), (^?), (^?!))
import           Network.URI
import           Network.Wreq.Lens    (responseBody)
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Xml.Lens        as Xml

import           Debug.Trace          as Debug

import           Daimust
import           Daimust.Crawler


data Settings =
  Settings
  { loginUrl :: URI
  , username :: Text
  , password :: Text
  }
  deriving (Eq, Show)


run :: IO ()
run = do
  settings@Settings {..} <- readSettings
  Debug.traceShow loginUrl $ pure ()

  res <- runCrawler $ do
    login settings
      >>= gotoEntrance

  let tables = fmap parseTable . lastMay $ res ^.. responseBody . html . selected "table"
  void $ (traverse . traverse) (putStrLn . unwords) tables

  pure ()

login :: Settings -> Crawler Response
login Settings {..} = do
  res <- get loginUrl
  let form = res ^?! responseBody . html . forms
      form' = form
              & fields . at "PN_ID" ?~ username
              & fields . at "PN_PASS" ?~ password
  submit form'

gotoEntrance :: Response -> Crawler Response
gotoEntrance res = do
  let form' = res ^?! responseBody . html . forms
              & fields . at "ACTION" ?~ "3"
  res' <- submit form'

  let Just (action', username, password) = do
        onload <- res' ^. responseBody . html . selected "body" . attr "onLoad"
        [u, p, url] <- flip (parseMaybe @()) onload $ do
          void $ many (notChar '(')
          between (char '(') (char ')') $ many (
            between (char '\'') (char '\'') (many (notChar '\'')) <* many (char ',')
            )
        (,,) <$> parseURIReference url <*> pure (pack u) <*> pure (pack p)

  let form' = res' ^?! responseBody . html . forms
              & action .~ action'
              & fields . at "pn0001" ?~ username
              & fields . at "pn0002" ?~ password
  res' <- submit form'

  let Just src' = lastMay $ res' ^.. responseBody . html . frames . src
  res' <- get src'
  -- traverse_ printLink $ res' ^.. responseBody . html . links

  let Just link' = lastMay $ res' ^.. responseBody . html . links
  res' <- click link'
  -- traverse_ printLink $ res' ^.. responseBody . html . links

  pure res'


readSettings :: IO Settings
readSettings = do
  Just loginUrl <- parseURI <$> getEnv "DAIM_URL"
  username <- pack <$> getEnv "DAIM_USERNAME"
  password <- pack <$> getEnv "DAIM_PASSWORD"
  pure Settings {..}


parseTable :: Dom -> [[Text]]
parseTable table = do
  tr <- table ^.. selected "tr"
  pure $ do
    td <- tr ^.. selected "td"
    let text' = td ^. folding universe . text
    pure $ bool "__" (unwords $ words text') ((length . filter (not . null . concat . words) $ lines text') == 1)
