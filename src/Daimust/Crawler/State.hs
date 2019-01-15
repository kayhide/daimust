module Daimust.Crawler.State
  ( State (..)
  , dumpState
  , restoreState
  )
where

import           ClassyPrelude

import qualified Data.Binary             as Binary
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.URI
import           Network.Wreq.Session    (Session, getSessionCookieJar,
                                          newSessionControl)


data State =
  State
  { session :: Session
  , url     :: URI
  }
  deriving (Show)


type DumpState = (Text, Text)

dumpState :: MonadIO m => State -> m ByteString
dumpState State {..} = do
  cookies <- liftIO $ getSessionCookieJar session
  pure . toStrict $ Binary.encode (tshow cookies, tshow url)

restoreState :: MonadIO m => ByteString -> m (Maybe State)
restoreState bs = do
  let ds = Binary.decode $ fromStrict bs :: DumpState
  let (cookies, url') = (join . readMay *** parseURI . unpack) ds
  case url' of
    Just url -> do
      session <- liftIO $ newSessionControl cookies tlsManagerSettings
      pure . Just $ State {..}
    _ -> pure Nothing



