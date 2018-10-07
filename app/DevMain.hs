module DevMain
where

import           ClassyPrelude        hiding (many, some)

import           Control.Lens         (at, folding, to, universe, (&), (.~), _last,
                                       (?~), (^.), (^..), (^?), (^?!))
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


main :: IO ()
main = do
  settings@Settings {..} <- readSettings
  Debug.traceShow loginUrl $ pure ()

  res <- runCrawler $ do
    login settings
      >>= gotoEntrance

  let trs = res ^.. responseBody . html . selected "tr"
  traverse_ (putStrLn . intercalate ", " . (^.. folding universe . text . to (unwords . words))) trs
  traverse_ (putStrLn . unwords . words . unwords . (^.. folding universe . text)) trs

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
