module Daimust.Daim
  ( Attendance (..)
  , Period (..)
  , ClientMonad
  , runClient
  , authenticate
  , headerTexts
  , getCurrentPeriod
  , moveToPeriod
  , listAttendances
  , updateAttendance
  , deleteAttendance
  )
where

import           ClassyPrelude             hiding (many, some)

import           Control.Lens              (at, folding, indices, ix, only, to,
                                            traversed, universe, view, (&),
                                            (.~), (?~), (^.), (^..), (^?),
                                            (^?!), _1, _2, _Just)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.State       (StateT, evalStateT, get, gets, put)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Default              (def)
import           Data.Void                 (Void)
import           Network.URI               (URI (..), parseURIReference)
import           Network.Wreq.Lens         (responseBody)
import           Path                      (Abs, File, Path, toFilePath)
import           Path.IO                   (doesFileExist)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Xml.Lens

import           Debug.Trace               as Debug

import           Configurable              (HasConfig, RIO, setting)
import           Daimust.Crawler           (Crawler, Dom, Response, URI, action,
                                            dom, fields, forms, frames,
                                            getState, links, printForm,
                                            putState, refresh, runCrawler,
                                            selected, src)
import qualified Daimust.Crawler           as Crawler
import           Daimust.Daim.Config
import           Daimust.Data.Attendance
import           Daimust.Data.Period
import           Daimust.Display
import qualified Daimust.Paths             as Paths
import           Daimust.Paths.Config      (PathsConfig)
import qualified Plugin.Logger             as Logger
import           Plugin.Logger.Config      (LoggerConfig)


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
  { basePage  :: Maybe Response
  , state     :: Crawler.State
  , cacheFile :: Maybe (Path Abs File)
  }
  deriving (Show)


-- * Client monad

type ClientMonad env a =
  ( HasConfig env LoggerConfig
  , HasConfig env DaimConfig
  )
  => StateT Client (RIO env) a

runClient
  :: ( HasConfig env LoggerConfig
     , HasConfig env PathsConfig
     , HasConfig env DaimConfig
     )
  => ClientMonad env a -> RIO env a
runClient action' = do
  state <- runCrawler getState
  cacheFile' <- Paths.getCacheFile
  evalStateT action' Client { basePage = Nothing, cacheFile = Just cacheFile', .. }


-- * Operations

authenticate :: ClientMonad env Response
authenticate =
  gets basePage
  >>= maybe tryRestore (pure . Just)
  >>= maybe go pure
  where
    tryRestore :: ClientMonad env (Maybe Response)
    tryRestore = do
      uncacheState
      gets basePage

    go :: ClientMonad env Response
    go = do
      Client {..} <- get
      setting' <- view (setting @_ @DaimConfig)
      Logger.info "Authenticating"
      (res, state') <- runCrawler $ do
        res <- gotoEntrance =<< login setting'
        state' <- getState
        pure (res, state')
      put Client { basePage = Just res, state = state', .. }
      cacheState
      pure res

headerTexts :: ClientMonad env [Text]
headerTexts = do
  page <- authenticate
  pure $ fromMaybe (pure []) $ do
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    let DisplayTableConfig { headerRows } = attendancesTable
    pure $ (table ^.. selected "tr") ^.. traversed . indices (`elem` headerRows) . to (unwords . rowTexts)

getCurrentPeriod :: ClientMonad env (Maybe Period)
getCurrentPeriod = do
  page <- authenticate
  Client {..} <- get
  (res, state') <- runCrawler $ do
    putState state
    res <- gotoCurrentPeriod page
    state' <- getState
    pure (res, state')
  put Client { basePage = Just res, state = state', .. }
  pure $ getPeriod res

moveToPeriod :: Period -> ClientMonad env ()
moveToPeriod period' = do
  page <- authenticate
  let current = getPeriod page
  when (current /= Just period') $ do
    Client {..} <- get
    (res, state') <- runCrawler $ do
      putState state
      res <- gotoPeriod period' page
      state' <- getState
      pure (res, state')
    put Client { basePage = Just res, state = state', .. }

listAttendances :: ClientMonad env [Attendance]
listAttendances = do
  page <- authenticate
  pure $ fromMaybe [] $ do
    period' <- getPeriod page
    table <- lastMay $ page ^.. responseBody . html . selected "table"
    pure . catMaybes $ parseItem period' <$> table ^.. selected "tr"

updateAttendance :: Attendance -> ClientMonad env ()
updateAttendance att = do
  page <- authenticate
  Client { .. } <- get
  Logger.info $ "Updating: Attendane #" <> att ^. date
  client' <- runCrawler $ do
    putState state
    res <- postUpdate att page
    state' <- getState
    pure Client { basePage = Just res, state = state', .. }
  put client'

deleteAttendance :: Attendance -> ClientMonad env ()
deleteAttendance att = do
  page <- authenticate
  Client { .. } <- get
  Logger.info $ "Deleting: Attendance #" <> att ^. date
  client' <- runCrawler $ do
    putState state
    res <- postDelete att page
    state' <- getState
    pure Client { basePage = Just res, state = state', .. }
  put client'


-- * Cache

cacheState :: ClientMonad env ()
cacheState = void $ runMaybeT $ do
  file <- MaybeT $ gets cacheFile
  lift $ do
    Logger.info "Caching State"
    gets state
      >>= Crawler.dumpState
      >>= writeFile (toFilePath file)

uncacheState :: ClientMonad env ()
uncacheState = void $ runMaybeT $ do
  file <- MaybeT $ gets cacheFile
  guard =<< doesFileExist file
  lift $ Logger.info "Uncaching State"
  state' <- MaybeT $ Crawler.restoreState =<< readFile (toFilePath file)
  res :: Response <- runCrawler $ do
    putState state'
    refresh
  guard $ isEntrance res
  lift $ do
    Client {..} <- get
    put Client { basePage = Just res, state = state', .. }


-- * Crawler actions

login :: DaimSetting -> Crawler Response
login DaimSetting {..} = do
  res <- Crawler.get _url
  let form = res ^?! responseBody . html . forms
      form' = form
              & fields . at "PN_ID" ?~ _username
              & fields . at "PN_PASS" ?~ _password
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

    let Just (action', username', password') = do
          onload <- res1 ^. responseBody . html . selected "body" . attr "onLoad"
          [u, p, url'] <- (parseMaybe @()) onloadP onload
          url'' <- parseURIReference url'
          pure (url'', pack u, pack p)

    let form' = res1 ^?! responseBody . html . forms
                & action .~ action'
                & fields . at "pn0001" ?~ username'
                & fields . at "pn0002" ?~ password'
    Crawler.submit form'

  res3 <- do
    let Just src' = lastMay $ res2 ^.. responseBody . html . frames . src
    Crawler.get src'
  -- traverse_ printLink $ res3 ^.. responseBody . html . links

  res4 <- do
    let Just link' = lastMay $ res3 ^.. responseBody . html . links
    Crawler.click link'
  -- traverse_ Crawler.printLink $ res4 ^.. responseBody . html . links
  -- traverse_ Crawler.printForm $ res4 ^.. responseBody . html . forms

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
              & fields . at "pn10s11" ?~ att ^. attendity . _Just . _1 <> ","
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
  verifyResponse =<< Crawler.submit form'

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
      verifyResponse =<< Crawler.submit form'

    delete' res' = do
      let form01 = findForm "form01" res'
      let form02 = findForm "form02" res'
      let form' = form01
                  & fields . at "pn00010" ?~ att ^. date
                  & fields . at "pn00011" .~ (form02 ^. fields . at "TEMP_pn00011")
                  & fields . at "pn00012" ?~ att ^. date <> att ^. enter <> "00"
                  & fields . at "pn10009" .~ (form02 ^. fields . at "TEMP_pn10009")
                  & fields . at "pn10102" ?~ "rec_del"
      verifyResponse =<< Crawler.submit form'


gotoPeriod :: Period -> Response -> Crawler Response
gotoPeriod period' res = do
  let form01 = findForm "form01" res
  let form' = form01
              & fields . at "pn10100" ?~ period' ^. year . to tshow
              & fields . at "pn10101" ?~ period' ^. month . to tshow
              & fields . at "pn10102" ?~ "search"
  verifyResponse =<< Crawler.submit form'

gotoCurrentPeriod :: Response -> Crawler Response
gotoCurrentPeriod res = do
  let form01 = findForm "form01" res
  let url' = form01 ^. action
  verifyResponse =<< Crawler.get url' { uriQuery = "?pn10102=Default" }

-- * Helper functions

verifyResponse :: MonadFail m => Response -> m Response
verifyResponse res = do
  let err = headMay . drop 1 $
            res ^.. responseBody . html . selected "font" . attributed (ix "color" . only "red")
  case err ^? _Just . text of
    Just msg -> fail $ unpack msg
    Nothing  -> pure res


isEntrance :: Response -> Bool
isEntrance res = isJust $ getPeriod res

findForm :: Text -> Response -> Crawler.Form
findForm name' res =
  res ^?! responseBody . html . selected "form" . attributed (ix "name" . only name') . forms

getPeriod :: Response -> Maybe Period
getPeriod res = do
  let form01 = findForm "form01" res
  year' <- readMay =<< form01 ^. fields . at "pn10100"
  month' <- readMay =<< form01 ^. fields . at "pn10101"
  pure $ Period year' month'

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
    let attendity' = do
          value' <- (tr ^.. selected "td") ^. ix 12 . selected "input" . attr "value"
          toAttendity value' $ cells ^? ix 12
    color' <- headMay . catMaybes $ tr ^.. selected "td" . attr "bgcolor"
    date' <- parseMaybe dateParser comment
    pure $ Attendance period' date' day' dow' enter' leave' attendity' color'

  where
    dateParser :: Parsec Void Text Text
    dateParser = do
      between (space *> string "ymd[") (string "]" *> space) $ do
        y <- some digitChar <* char '-'
        m <- some digitChar <* char '-'
        d <- some digitChar <* char '-'
        void $ some (notChar ']')
        pure $ pack y <> pack m <> pack d
