{-# LANGUAGE TemplateHaskell #-}

module Daimust.Crawler.Type
  ( Response
  , Dom
  , Form (..)
  , Input (..)
  , Link (..)
  , Frame (..)
  , HasAction
  , HasFields
  , HasDom
  , HasKey
  , HasValue
  , HasHref
  , HasSrc
  , action
  , fields
  , dom
  , key
  , value
  , href
  , src
  )
where

import           ClassyPrelude

import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import           Network.URI          (URI)
import qualified Network.Wreq         as Wreq
import qualified Text.Xml.Lens        as Xml


type Response = Wreq.Response BL.ByteString
type Dom = Xml.Element

data Form = Form
  { _formAction :: URI
  , _formFields :: Map Text Text
  , _formDom    :: Dom
  } deriving (Show, Generic)

makeFields ''Form


data Input = Input
  { _inputKey   :: Text
  , _inputValue :: Text
  , _inputDom   :: Dom
  } deriving (Show, Generic)

makeFields ''Input


data Link = Link
  { _linkHref :: URI
  , _linkDom  :: Dom
  } deriving (Show, Generic)

makeFields ''Link


data Frame = Frame
  { _frameSrc :: URI
  , _frameDom :: Dom
  } deriving (Show, Generic)

makeFields ''Frame
