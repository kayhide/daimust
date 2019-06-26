module Daimust.Crawler.Utils
  ( formatElement
  , formatForm
  , formatLink
  )
where

import ClassyPrelude

import Control.Lens (to, (.~), (^.))
import Data.Default (def)
import qualified Data.Map as Map
import Text.XML (Document(Document), Prologue(Prologue), rsPretty, renderLBS)
import qualified Text.XML as Xml
import Text.XML.Lens (localName)

import Daimust.Crawler.Lens (domClass, domId, innerText)
import Daimust.Crawler.Type (Form, Link, action, dom, fields, href)


-- * Formmatting dom element

formatElement :: Xml.Element -> Text
-- formatElement x = toStrict $ x ^. Xml.renderWith (rsPretty .~ True)
formatElement x =
  let renderSettings = def { rsPretty = True }
      doc = elementToDocument x
      rendered = renderLBS renderSettings doc
  in decodeUtf8 $ toStrict rendered

elementToDocument :: Xml.Element -> Document
elementToDocument e = Document (Prologue [] Nothing []) e []

formatForm :: Form -> [Text]
formatForm form =
  [ mconcat
    [ form ^. dom . localName
    , form ^. domId . to ("#" <>)
    , form ^. domClass . to ("." <>)
    ]
  , tshow $ form ^. action
  ]
  <> (tshow <$> Map.toAscList (form ^. fields))
  -- <> [tshow (form ^. dom)]

formatLink :: Link -> Text
formatLink link' =
  mconcat
  [ link' ^. dom . localName
  , link' ^. domId . to ("#" <>)
  , link' ^. domClass . to ("." <>)
  ]
  <> "[" <> link' ^. innerText <> "]"
  <> "(" <> tshow (link' ^. href) <> ")"
  <> formatElement (link' ^. dom)
