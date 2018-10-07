{-# LANGUAGE TemplateHaskell #-}

module Daimust.Crawler.Type
where

import           ClassyPrelude

import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import           Network.URI          (URI)
import qualified Network.Wreq         as Wreq
import qualified Text.Xml.Lens        as Xml


type Response = Wreq.Response BL.ByteString

data Form = Form
  { _formAction :: URI
  , _formFields :: Map Text Text
  , _formDom    :: Xml.Element
  } deriving (Show, Generic)

makeFields ''Form


data Input = Input
  { _inputKey   :: Text
  , _inputValue :: Text
  , _inputDom   :: Xml.Element
  } deriving (Show, Generic)

makeFields ''Input


data Link = Link
  { _linkHref :: URI
  , _linkDom  :: Xml.Element
  } deriving (Show, Generic)

makeFields ''Link


data Frame = Frame
  { _frameSrc :: URI
  , _frameDom  :: Xml.Element
  } deriving (Show, Generic)

makeFields ''Frame
