module Daimust.Client
  ( Settings (..)
  , Attendance (..)
  , Client
  , newClient
  , ClientMonad
  , runClient
  , evalClient
  , execClient
  , setVerbose
  , isVerbose
  , authenticate
  , headerTexts
  , listAttendances
  , updateAttendance
  , deleteAttendance
  )
where

import           ClassyPrelude           hiding (many, some)

import           Control.Lens            (at, folded, folding, indices, ix,
                                          only, to, traversed, universe, (&),
                                          (...), (.~), (?~), (^.), (^..), (^?),
                                          (^?!), _Just, _last)
import           Control.Monad.State     (StateT, evalStateT, execStateT, get,
                                          gets, modify, put, runStateT)
import           Data.Default            (def)
import           Network.URI             (parseURIReference)
import           Network.Wreq.Lens       (responseBody)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Xml.Lens

import           Debug.Trace             as Debug

import           Daimust.Crawler         (Crawler, Dom, Response, URI, action,
                                          dom, fields, forms, frames, getState,
                                          links, printForm, putState,
                                          runCrawler, selected, src)
import qualified Daimust.Crawler         as Crawler
import           Daimust.Data.Attendance (Attendance (..), parseHours)
import           Daimust.Display


-- * Public data types

data Settings =
  Settings
  { loginUrl :: URI
  , username :: Text
  , password :: Text
  }
  deriving (Eq, Show)


-- * Table config

attendancesTable :: DisplayTableConfig
attendancesTable =
  def
  { headerRows = [0]
  , dropRows = [1, 2]
  , pickColumns = Just [0, 1, 10, 12]
  }


-- * Client data type

data Client =
  Client
  { settings :: Settings
  , basePage :: Maybe Response
  , state    :: Crawler.State
  , verbose  :: Bool
  }

newClient :: Settings -> IO Client
newClient settings = do
  state <- runCrawler getState
  pure Client { basePage = Nothing, verbose = True, ..}


-- * Client monad

type ClientMonad a = StateT Client IO a

runClient :: ClientMonad a -> Client -> IO (a, Client)
runClient = runStateT

evalClient :: ClientMonad a -> Client -> IO a
evalClient = evalStateT

execClient :: ClientMonad a -> Client -> IO Client
execClient = execStateT


-- * Operations

setVerbose :: Bool -> ClientMonad ()
setVerbose b =
  modify $ \client -> client { verbose = b }

isVerbose :: ClientMonad Bool
isVerbose = gets verbose

authenticate :: ClientMonad Response
authenticate = do
  Client {..} <- get
  case basePage of
    Nothing -> do
      (client', res) <- liftIO $ runCrawler $ do
        res <- gotoEntrance =<< login settings
        state' <- getState
        pure (Client { basePage = Just res, state = state', .. }, res)
      put client'
      pure res
    Just res -> pure res

headerTexts :: ClientMonad [Text]
headerTexts = do
  page <- authenticate
  pure $ fromMaybe (pure []) $ do
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    let DisplayTableConfig { headerRows } = attendancesTable
    pure $ (table ^.. selected "tr") ^.. traversed . indices (`elem` headerRows) . to (unwords . rowTexts)

listAttendances :: ClientMonad [Attendance]
listAttendances = do
  page <- authenticate
  pure $ fromMaybe [] $ do
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    pure . catMaybes $ parseItem <$> table ^.. selected "tr"

updateAttendance :: Attendance -> ClientMonad ()
updateAttendance att = do
  page <- authenticate
  Client { .. } <- get
  client' <- liftIO $ runCrawler $ do
    putState state
    res <- postUpdate att page
    state' <- getState
    pure Client { basePage = Just res, state = state', .. }
  put client'

deleteAttendance :: Attendance -> ClientMonad ()
deleteAttendance att = do
  page <- authenticate
  Client { .. } <- get
  client' <- liftIO $ runCrawler $ do
    putState state
    res <- postDelete att page
    state' <- getState
    pure Client { basePage = Just res, state = state', .. }
  put client'

-- * Low level functions

login :: Settings -> Crawler Response
login Settings {..} = do
  res <- Crawler.get loginUrl
  let form = res ^?! responseBody . html . forms
      form' = form
              & fields . at "PN_ID" ?~ username
              & fields . at "PN_PASS" ?~ password
  Crawler.submit form'

gotoEntrance :: Response -> Crawler Response
gotoEntrance res = do
  res1 <- do
    let form' = res ^?! responseBody . html . forms
                & fields . at "ACTION" ?~ "3"
    Crawler.submit form'

  res2 <- do
    let onloadP = do
          void $ many (notChar '(')
          between (char '(') (char ')') $ many (
            between (char '\'') (char '\'') (many (notChar '\'')) <* many (char ',')
            )

    let Just (action', username, password) = do
          onload <- res1 ^. responseBody . html . selected "body" . attr "onLoad"
          [u, p, url] <- (parseMaybe @()) onloadP onload
          url' <- parseURIReference url
          pure (url', pack u, pack p)

    let form' = res1 ^?! responseBody . html . forms
                & action .~ action'
                & fields . at "pn0001" ?~ username
                & fields . at "pn0002" ?~ password
    Crawler.submit form'

  res3 <- do
    let Just src' = lastMay $ res2 ^.. responseBody . html . frames . src
    Crawler.get src'
  -- traverse_ printLink $ res3 ^.. responseBody . html . links

  res4 <- do
    let Just link' = lastMay $ res3 ^.. responseBody . html . links
    Crawler.click link'
  -- traverse_ printLink $ res4 ^.. responseBody . html . links
  -- traverse_ printForm $ res4 ^.. responseBody . html . forms

  pure res4


findForm :: Text -> Response -> Crawler.Form
findForm name res =
  res ^?! responseBody . html . selected "form" . attributed (ix "name" . only name) . forms


postUpdate :: Attendance -> Response -> Crawler Response
postUpdate Attendance {..} res = do
  let form02 = findForm "form02" res
  let form' = form02
              & fields . at "pn10s01" ?~ date <> ","
              & fields . at "pn10s02" ?~ date <> ","
              & fields . at "pn10s03" .~ (form02 ^. fields . at "TEMP_pn00011")
              & fields . at "pn10s04" .~ (form02 ^. fields . at "TEMP_pn10009")
              & fields . at "pn10s05" ?~ ","
              & fields . at "pn10s06" ?~ "off,"
              & fields . at "pn10s06t" ?~ "off,"
              & fields . at "pn10s07" ?~ ","
              & fields . at "pn10s07t" ?~ ","
              & fields . at "pn10s08" ?~ ","
              & fields . at "pn10s08t" ?~ ","
              & fields . at "pn10s09" ?~ date <> enter <> ","
              & fields . at "pn10s10" ?~ leave <> ","
              & fields . at "pn10s11" ?~ noteValue <> ","
              & fields . at "pn10s14" ?~ ","
              & fields . at "pn10s15" ?~ ","
              & fields . at "pn10s28" ?~ ","
              & fields . at "pn10s29" ?~ ","
              & fields . at "pn10s30" ?~ ","
              & fields . at "pn10s31" ?~ ","
              & fields . at "pn10s32" ?~ ","
              & fields . at "pn10s33" ?~ ","
              & fields . at "pn10s35" ?~ ","
              & fields . at "pn10s38" ?~ ","
  Crawler.submit form'

postDelete :: Attendance -> Response -> Crawler Response
postDelete Attendance {..} = predelete' >=> delete'
  where
    predelete' res' = do
      let form01 = findForm "form01" res'
      let form02 = findForm "form02" res'
      let form' = form01
                  & fields . at "pn00010" ?~ date
                  & fields . at "pn00011" .~ (form02 ^. fields . at "TEMP_pn00011")
                  & fields . at "pn00012" ?~ date <> enter <> "00"
                  & fields . at "pn10009" .~ (form02 ^. fields . at "TEMP_pn10009")
                  & fields . at "pn10102" ?~ "mishonin"
      Crawler.submit form'

    delete' res' = do
      let form01 = findForm "form01" res'
      let form02 = findForm "form02" res'
      let form' = form01
                  & fields . at "pn00010" ?~ date
                  & fields . at "pn00011" .~ (form02 ^. fields . at "TEMP_pn00011")
                  & fields . at "pn00012" ?~ date <> enter <> "00"
                  & fields . at "pn10009" .~ (form02 ^. fields . at "TEMP_pn10009")
                  & fields . at "pn10102" ?~ "rec_del"
      Crawler.submit form'



rowTexts :: Dom -> [Text]
rowTexts tr = do
  td <- tr ^.. selected "td"
  let text' = td ^. folding universe . text
  pure $ bool "" (unwords $ words text') ((length . filter (not . null . concat . words) $ lines text') == 1)

parseItem :: Dom -> Maybe Attendance
parseItem tr = headMay . catMaybes $ do
  comment <- tr ^.. selected "" . comments
  pure $ do
    let cells = rowTexts tr
    day <- cells ^? ix 0
    dow <- cells ^? ix 1
    hours <- cells ^? ix 10
    let (enter, leave) = fromMaybe ("", "") $ parseHours hours
    let noteValue = fromMaybe "" $ (tr ^.. selected "td") ^. ix 12 . selected "input" . attr "value"
    noteLabel <- cells ^? ix 12
    flip (parseMaybe @()) comment $ do
      between (space *> string "ymd[") (string "]" *> space) $ do
        y <- some digitChar <* char '-'
        m <- some digitChar <* char '-'
        d <- some digitChar <* char '-'
        void $ some (notChar ']')
        pure $ Attendance { date = pack y <> pack m <> pack d, .. }
