module Daimust.Crawler.Monad
  ( Crawler
  , State
  , runCrawler
  , getState
  , putState
  , currentUrl
  , get
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
import           Network.URI
import           Network.Wreq              (FormParam (..))
import           Network.Wreq.Session      (Session)
import qualified Network.Wreq.Session      as Session
import           Text.Xml.Lens             as Xml

import           Daimust.Crawler.Type
import           Daimust.Crawler.Lens


data CrawlerI a where
  CurrentUrl :: CrawlerI URI
  Get :: URI -> CrawlerI Response
  Submit :: Form -> CrawlerI Response
  Click :: Link -> CrawlerI Response

  GetState :: CrawlerI State
  PutState :: State -> CrawlerI ()

  PrintElement :: Xml.Element -> CrawlerI ()
  PrintForm :: Form -> CrawlerI ()
  PrintLink :: Link -> CrawlerI ()


type Crawler a = Program CrawlerI a

data State =
  State
  { session :: Session
  , url     :: URI
  }
  deriving (Show)

runCrawler :: Crawler a -> IO a
runCrawler m = do
  session <- Session.newSession
  runCrawler' State {..} m
  where
    url = URI "" Nothing "" "" ""

runCrawler' :: State -> Crawler a -> IO a
runCrawler' state@State {..} = eval . Op.view
  where
    eval :: ProgramView CrawlerI a -> IO a

    eval (Return x) = pure x

    eval (CurrentUrl :>>= k) =
      pure url
      >>= runCrawler' state . k

    eval (Get url' :>>= k) =
      Session.get session (show url')
      >>= runCrawler' State { url = url', .. } . k

    eval (Submit form :>>= k) =
      Session.post session (show url') xs'
      >>= runCrawler' State { url = url', .. } . k
      where
        url' = form ^. action
        xs' =
          uncurry (:=) . first encodeUtf8 <$> form ^. fields . to Map.toList

    eval (Click link' :>>= k) =
      Session.get session (show url')
      >>= runCrawler' State { url = url', .. } . k
      where
        url' = link' ^. href

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

submit :: Form -> Crawler Response
submit form = do
  url <- currentUrl
  Op.singleton $ Submit $
    form & action %~ flip relativeTo url

click :: Link -> Crawler Response
click link' = do
  url <- currentUrl
  Op.singleton $ Click $
    link' & href %~ flip relativeTo url

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
printLink' link = do
  putStrLn $ mconcat
    [ link ^. dom . name
    , link ^. domId . to ("#" <>)
    , link ^. domClass . to ("." <>)
    ]
  print $ link ^. href
  putStrLn $ link ^. innerText
  -- print' $ link ^. dom
