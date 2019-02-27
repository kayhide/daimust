{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Configurable where

import           ClassyPrelude

import           Control.Lens          (Lens')
import           Data.Extensible       (Forall, Member (..), item)
import           Data.Extensible.Plain (AllOf, (<%))
import           Data.Proxy            (Proxy (..))
import           System.Environment    (lookupEnv)


class Configurable (a :: *) where
  type Setting a = r | r -> a
  type Running a = r | r -> a
  type Deps a :: [*]

  ready :: IO (Setting a)

  start
    :: ( Forall Configurable (Deps a)
       , Forall (HasConfig env) (Deps a)
       , env ~ AllOf confs
       )
    => Setting a
    -> env
    -> IO (Running a)

class Configurable a => HasConfig env a where
  setting :: Lens' env (Setting a)
  running :: Lens' env (Running a)

instance ( Configurable a
         , Member configs (Setting a)
         , Member configs (Running a)
         ) => HasConfig (AllOf configs) a where
  setting = item (Proxy @(Setting a))
  running = item (Proxy @(Running a))

type family ToConfigs as = r | r -> as where
  ToConfigs '[] = '[]
  ToConfigs (a ': as) = Setting a ': Running a ': ToConfigs as


activate
  :: ( Configurable a
     , Forall Configurable (Deps a)
     , Forall (HasConfig env) (Deps a)
     , env ~ AllOf confs
     )
  => env -> IO (AllOf (Setting a ': Running a ': confs))
activate env = do
  setting' <- ready
  running' <- start setting' env
  pure $ setting' <% running' <% env

activateWith
  :: ( Configurable a
     , Forall Configurable (Deps a)
     , Forall (HasConfig env) (Deps a)
     , env ~ AllOf confs
     )
  => (Setting a -> Setting a) ->env -> IO (AllOf (Setting a ': Running a ': confs))
activateWith modify env = do
  setting' <- modify <$> ready
  running' <- start setting' env
  pure $ setting' <% running' <% env


class FetchSetting a where
  fetchSetting :: Text -> a -> IO a
  default fetchSetting :: (Read a) => Text -> a -> IO a
  fetchSetting key def =
    fromMaybe def . (readMay =<<) <$> lookupEnv (unpack key)

instance FetchSetting Int

instance FetchSetting Bool where
  fetchSetting key def = do
    let falses = ["false", "f", "0"] :: [Text]
    maybe def ((`notElem` falses) . toLower . pack) <$> lookupEnv (unpack key)

instance FetchSetting Text where
  fetchSetting key def =
    maybe def pack <$> lookupEnv (unpack key)

instance FetchSetting [Text] where
  fetchSetting key def =
    maybe def (words . pack) <$> lookupEnv (unpack key)
