module Daimust.Crawler.Utils
  ( formatElement
  , formatForm
  , formatLink
  )
where

import           ClassyPrelude

import           Control.Lens              (to, (.~), (^.))
import qualified Data.Map                  as Map
import           Text.Xml.Lens             as Xml

import           Daimust.Crawler.Lens
import           Daimust.Crawler.Type


-- * Formmatting dom element

formatElement :: Xml.Element -> Text
formatElement x = toStrict $ x ^. Xml.renderWith (rsPretty .~ True)

formatForm :: Form -> [Text]
formatForm form =
  [ mconcat
    [ form ^. dom . name
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
  [ link' ^. dom . name
  , link' ^. domId . to ("#" <>)
  , link' ^. domClass . to ("." <>)
  ]
  <> "[" <> link' ^. innerText <> "]"
  <> "(" <> tshow (link' ^. href) <> ")"
  <> formatElement (link' ^. dom)
