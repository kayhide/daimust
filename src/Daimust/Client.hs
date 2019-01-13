module Daimust.Client
  ( Settings (..)
  , Attendance (..)
  , Period (..)
  , Client
  , newClient
  , ClientMonad
  , runClient
  , evalClient
  , execClient
  , setVerbose
  , isVerbose
  , setCacheFile
  , getCacheFile
  , authenticate
  , headerTexts
  , getCurrentPeriod
  , moveToPeriod
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
import           Network.Wreq.Session    (Session (..), getSessionCookieJar)
import           Path                    (Abs, File, Path, toFilePath)
import           System.IO.Unsafe        (unsafePerformIO)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Xml.Lens

import           Debug.Trace             as Debug

import           Daimust.Crawler         (Crawler, Dom, Response, URI, action,
                                          dom, fields, forms, frames, getState,
                                          links, printForm, putState, refresh,
                                          runCrawler, selected, src)
import qualified Daimust.Crawler         as Crawler
import           Daimust.Data.Attendance
import           Daimust.Data.Period
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
  { settings  :: Settings
  , basePage  :: Maybe Response
  , state     :: Crawler.State
  , verbose   :: Bool
  , cacheFile :: Maybe (Path Abs File)
  }
  deriving (Show)

newClient :: Settings -> IO Client
newClient settings = do
  state <- runCrawler getState
  pure Client { basePage = Nothing, verbose = False, cacheFile = Nothing, ..}


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

setCacheFile :: Path Abs File -> ClientMonad ()
setCacheFile file =
  modify $ \client -> client { cacheFile = Just file }

getCacheFile :: ClientMonad (Maybe (Path Abs File))
getCacheFile = gets cacheFile

cacheState :: ClientMonad ()
cacheState = do
  Client {..} <- get
  traverse_ (go state) cacheFile
  where
    go :: Crawler.State -> Path Abs File -> ClientMonad ()
    go state file = do
      sayInfo "Caching State"
      writeFile (toFilePath file) =<< Crawler.dumpState state

uncacheState :: ClientMonad ()
uncacheState = do
  traverse_ go =<< gets cacheFile
  where
    go :: Path Abs File -> ClientMonad ()
    go file = do
      sayInfo "Uncaching State"
      readFile (toFilePath file)
        >>= Crawler.restoreState
        >>= \case
        Nothing -> pure ()
        Just state' -> do
          res <- liftIO $ runCrawler $ do
            putState state'
            refresh
          when (isEntrance res) $ do
            Client {..} <- get
            put Client { basePage = Just res, state = state', .. }

authenticate :: ClientMonad Response
authenticate = do
  gets basePage
  >>= maybe tryRestore (pure . Just)
  >>= maybe go pure
  where
    tryRestore :: ClientMonad (Maybe Response)
    tryRestore = do
      uncacheState
      gets basePage

    go :: ClientMonad Response
    go = do
      Client {..} <- get
      sayInfo "Authenticating"
      (res, state') <- liftIO $ runCrawler $ do
        res <- gotoEntrance =<< login settings
        state' <- getState
        pure (res, state')
      put Client { basePage = Just res, state = state', .. }
      cacheState
      pure res

headerTexts :: ClientMonad [Text]
headerTexts = do
  page <- authenticate
  pure $ fromMaybe (pure []) $ do
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    let DisplayTableConfig { headerRows } = attendancesTable
    pure $ (table ^.. selected "tr") ^.. traversed . indices (`elem` headerRows) . to (unwords . rowTexts)

getCurrentPeriod :: ClientMonad (Maybe Period)
getCurrentPeriod = do
  getPeriod <$> authenticate

moveToPeriod :: Period -> ClientMonad ()
moveToPeriod period = do
  page <- authenticate
  let current = getPeriod page
  when (current /= Just period) $ do
    Client {..} <- get
    client' <- liftIO $ runCrawler $ do
      putState state
      res <- gotoPeriod period page
      state' <- getState
      pure Client { basePage = Just res, state = state', .. }
    put client'

listAttendances :: ClientMonad [Attendance]
listAttendances = do
  page <- authenticate
  pure $ fromMaybe [] $ do
    period <- getPeriod page
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    pure . catMaybes $ parseItem period <$> table ^.. selected "tr"

updateAttendance :: Attendance -> ClientMonad ()
updateAttendance att = do
  page <- authenticate
  Client { .. } <- get
  sayInfo $ "Updating: Attendane #" <> att ^. date
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
  sayInfo $ "Deleting: Attendance #" <> att ^. date
  client' <- liftIO $ runCrawler $ do
    putState state
    res <- postDelete att page
    state' <- getState
    pure Client { basePage = Just res, state = state', .. }
  put client'


-- * Logging functions

sayInfo :: Text -> ClientMonad ()
sayInfo msg = do
  Client { verbose } <- get
  when verbose $ do
    liftIO $ putStrLn msg


-- * Crawler actions

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


postUpdate :: Attendance -> Response -> Crawler Response
postUpdate att res = do
  let form02 = findForm "form02" res
  let form' = form02
              & fields . at "pn10s01" ?~ att ^. date <> ","
              & fields . at "pn10s02" ?~ att ^. date <> ","
              & fields . at "pn10s03" .~ (form02 ^. fields . at "TEMP_pn00011")
              & fields . at "pn10s04" .~ (form02 ^. fields . at "TEMP_pn10009")
              & fields . at "pn10s05" ?~ ","
              & fields . at "pn10s06" ?~ "off,"
              & fields . at "pn10s06t" ?~ "off,"
              & fields . at "pn10s07" ?~ ","
              & fields . at "pn10s07t" ?~ ","
              & fields . at "pn10s08" ?~ ","
              & fields . at "pn10s08t" ?~ ","
              & fields . at "pn10s09" ?~ att ^. date <> att ^. enter <> ","
              & fields . at "pn10s10" ?~ att ^. leave <> ","
              & fields . at "pn10s11" ?~ att ^. noteValue <> ","
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
postDelete att = predelete' >=> delete'
  where
    predelete' res' = do
      let form01 = findForm "form01" res'
      let form02 = findForm "form02" res'
      let form' = form01
                  & fields . at "pn00010" ?~ att ^. date
                  & fields . at "pn00011" .~ (form02 ^. fields . at "TEMP_pn00011")
                  & fields . at "pn00012" ?~ att ^. date <> att ^. enter <> "00"
                  & fields . at "pn10009" .~ (form02 ^. fields . at "TEMP_pn10009")
                  & fields . at "pn10102" ?~ "mishonin"
      Crawler.submit form'

    delete' res' = do
      let form01 = findForm "form01" res'
      let form02 = findForm "form02" res'
      let form' = form01
                  & fields . at "pn00010" ?~ att^. date
                  & fields . at "pn00011" .~ (form02 ^. fields . at "TEMP_pn00011")
                  & fields . at "pn00012" ?~ att^. date <> att ^. enter <> "00"
                  & fields . at "pn10009" .~ (form02 ^. fields . at "TEMP_pn10009")
                  & fields . at "pn10102" ?~ "rec_del"
      Crawler.submit form'


gotoPeriod :: Period -> Response -> Crawler Response
gotoPeriod period res = do
  let form01 = findForm "form01" res
  let form' = form01
              & fields . at "pn10100" ?~ period ^. year . to tshow
              & fields . at "pn10101" ?~ period ^. month . to tshow
              & fields . at "pn10102" ?~ "search"
  Crawler.submit form'


-- * Helper functions

isEntrance :: Response -> Bool
isEntrance res = isJust $ getPeriod res

findForm :: Text -> Response -> Crawler.Form
findForm name res =
  res ^?! responseBody . html . selected "form" . attributed (ix "name" . only name) . forms

getPeriod :: Response -> Maybe Period
getPeriod res = do
  let form01 = findForm "form01" res
  year <- readMay =<< form01 ^. fields . at "pn10100"
  month <- readMay =<< form01 ^. fields . at "pn10101"
  pure $ Period year month

rowTexts :: Dom -> [Text]
rowTexts tr = do
  td <- tr ^.. selected "td"
  let text' = td ^. folding universe . text
  pure $ bool "" (unwords $ words text') ((length . filter (not . null . concat . words) $ lines text') == 1)

parseItem :: Period -> Dom -> Maybe Attendance
parseItem period' tr = headMay . catMaybes $ do
  comment <- tr ^.. selected "" . comments
  pure $ do
    let cells = rowTexts tr
    day' <- cells ^? ix 0
    dow' <- cells ^? ix 1
    hours <- cells ^? ix 10
    let (enter', leave') = fromMaybe ("", "") $ parseHours hours
    let noteValue' = fromMaybe "" $ (tr ^.. selected "td") ^. ix 12 . selected "input" . attr "value"
    noteLabel' <- cells ^? ix 12
    color' <- headMay . catMaybes $ tr ^.. selected "td" . attr "bgcolor"
    flip (parseMaybe @()) comment $ do
      between (space *> string "ymd[") (string "]" *> space) $ do
        y <- some digitChar <* char '-'
        m <- some digitChar <* char '-'
        d <- some digitChar <* char '-'
        void $ some (notChar ']')
        pure $ Attendance period' (pack y <> pack m <> pack d) day' dow' enter' leave' noteValue' noteLabel' color'
