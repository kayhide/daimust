module Daimust.Crawler.Monad
  ( Crawler
  , State
  , dumpState
  , restoreState
  , runCrawler
  , getState
  , putState
  , currentUrl
  , get
  , refresh
  , submit
  , click
  , printElement
  , printForm
  , printLink
  )
where

import           ClassyPrelude

import           Control.Lens              (at, to, (%~), (&), (.~), (?~), (^.),
                                            (^..), (^?), (^?!))
import           Control.Monad.Operational (Program, ProgramView,
                                            ProgramViewT (..))
import qualified Control.Monad.Operational as Op
import qualified Data.Map                  as Map
import           Network.HTTP.Client       (getUri)
import           Network.URI
import           Network.Wreq              (FormParam (..), defaults)
import           Network.Wreq.Lens         (hrFinalRequest, hrFinalResponse)
import qualified Network.Wreq.Session      as Session
import           Text.Xml.Lens             as Xml

import           Daimust.Crawler.Lens
import           Daimust.Crawler.State     (State (..), dumpState, restoreState)
import           Daimust.Crawler.Type


data CrawlerI a where
  CurrentUrl :: CrawlerI URI
  Get :: URI -> CrawlerI Response
  Post :: URI -> [FormParam] -> CrawlerI Response

  GetState :: CrawlerI State
  PutState :: State -> CrawlerI ()

  PrintElement :: Xml.Element -> CrawlerI ()
  PrintForm :: Form -> CrawlerI ()
  PrintLink :: Link -> CrawlerI ()


type Crawler a = Program CrawlerI a

runCrawler :: Crawler a -> IO a
runCrawler m = do
  session <- Session.newSession
  runCrawler' State {..} m
  where
    url = URI "" Nothing "" "" ""

runCrawler' :: State -> Crawler a -> IO a
runCrawler' state@State {..} = eval . Op.view
  where
    request' :: String -> URI -> [FormParam] -> IO (Response, URI)
    request' method url' body = do
      hr <- case method of
        "GET" ->
          Session.customHistoriedMethodWith method defaults session (show url')
        "POST" ->
          Session.customHistoriedPayloadMethodWith method defaults session (show url') body
        _ ->
          error $ "Invalid http method: " <> method
      -- print $ hr ^. hrFinalRequest
      pure (hr ^. hrFinalResponse, getUri $ hr ^. hrFinalRequest)

    eval :: ProgramView CrawlerI a -> IO a

    eval (Return x) = pure x

    eval (CurrentUrl :>>= k) =
      pure url
      >>= runCrawler' state . k

    eval (Get url' :>>= k) = do
      (res, url'') <- request' "GET" url' []
      runCrawler' State { url = url'', .. } . k $ res

    eval (Post url' body' :>>= k) = do
      (res, url'') <- request' "POST" url' body'
      runCrawler' State { url = url'', .. } . k $ res

    eval (GetState :>>= k) =
      pure state
      >>= runCrawler' state . k

    eval (PutState state' :>>= k) =
      runCrawler' state' $ k ()

    eval (PrintElement elm :>>= k) =
      printElement' elm
      >>= runCrawler' state . k

    eval (PrintForm elm :>>= k) =
      printForm' elm
      >>= runCrawler' state . k

    eval (PrintLink elm :>>= k) =
      printLink' elm
      >>= runCrawler' state . k


currentUrl :: Crawler URI
currentUrl = Op.singleton CurrentUrl

get :: URI -> Crawler Response
get = Op.singleton . Get

refresh :: Crawler Response
refresh = get =<< currentUrl

submit :: Form -> Crawler Response
submit form = do
  url <- relativeTo (form ^. action) <$> currentUrl
  Op.singleton $ Post url body
  where
    body = uncurry (:=) . first encodeUtf8 <$> form ^. fields . to Map.toList

click :: Link -> Crawler Response
click link' = do
  url <- relativeTo (link' ^. href) <$> currentUrl
  Op.singleton $ Get url

printElement :: Xml.Element -> Crawler ()
printElement = Op.singleton . PrintElement

printForm :: Form -> Crawler ()
printForm = Op.singleton . PrintForm

printLink :: Link -> Crawler ()
printLink = Op.singleton . PrintLink


-- * State management

getState :: Crawler State
getState = Op.singleton GetState

putState :: State -> Crawler ()
putState = Op.singleton . PutState

-- * For debug

printElement' :: Xml.Element -> IO ()
printElement' x = putStr . toStrict $ x ^. Xml.renderWith (rsPretty .~ True)

printForm' :: Form -> IO ()
printForm' form = do
  putStrLn $ mconcat
    [ form ^. dom . name
    , form ^. domId . to ("#" <>)
    , form ^. domClass . to ("." <>)
    ]
  print $ form ^. action
  traverse_ print $ Map.toAscList $ form ^. fields
  -- print' $ form ^. dom
  putStrLn ""

printLink' :: Link -> IO ()
printLink' link' = do
  putStrLn $ mconcat
    [ link' ^. dom . name
    , link' ^. domId . to ("#" <>)
    , link' ^. domClass . to ("." <>)
    ]
  print $ link' ^. href
  putStrLn $ link' ^. innerText
  -- print' $ link' ^. dom
