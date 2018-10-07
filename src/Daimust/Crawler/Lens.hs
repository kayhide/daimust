module Daimust.Crawler.Lens
where

import           ClassyPrelude

import           Control.Lens                (Fold, filtered, folded, folding,
                                              ix, only, to, universe, view,
                                              (^.), (^..))
import qualified Data.Map                    as Map
import           Network.URI
import           Text.Xml.Lens               (attr, attributed, html, name,
                                              named, text)
import qualified Text.Xml.Lens               as Xml

import           Daimust.Crawler.DomSelector
import           Daimust.Crawler.Type


selected :: DomSelector -> Fold Xml.Element Xml.Element
selected (DomSelector factors) = folding universe . filtered' factors
  where
    filtered' [] = filtered (const True)
    filtered' (DomName x : xs) = named (only name') . filtered' xs
      where name' = fromString $ unpack x
    filtered' (DomId x : xs) = attributed (ix "id" . only x) . filtered' xs
    filtered' (DomClass x : xs) = attributed (ix "class" . to words . folded . only x) . filtered' xs


forms :: Fold Xml.Element Form
forms = selected "form" . _Form

_Form :: Fold Xml.Element Form
_Form = to toForm . folded

toForm :: Xml.Element -> Maybe Form
toForm element = case element ^. name of
  "form" -> Just $ Form (fromMaybe emptyUrl action') fields' element
  _      -> Nothing
  where
    action' =
      element ^. attr "action"
      >>= parseURIReference . unpack . unescapeHtmlEntity

    fields' = Map.fromList $ element ^.. inputs . to (view key &&& view value)

    emptyUrl = URI "" Nothing "" "" ""


inputs :: Fold Xml.Element Input
inputs = selected "input" . _Input

_Input :: Fold Xml.Element Input
_Input = to toInput . folded

toInput :: Xml.Element -> Maybe Input
toInput element = Input <$> name' <*> pure value' <*> pure element
  where
    name' = element ^. attr "name"
    value' = element ^. attr "value" . folded


links :: Fold Xml.Element Link
links = selected "a" . _Link

_Link :: Fold Xml.Element Link
_Link = to toLink . folded

toLink :: Xml.Element -> Maybe Link
toLink element = Link <$> href' <*> pure element
  where
    href' =
      element ^. attr "href"
      >>= parseURIReference . unpack . unescapeHtmlEntity


frames :: Fold Xml.Element Frame
frames = selected "frame" . _Frame

_Frame :: Fold Xml.Element Frame
_Frame = to toFrame . folded

toFrame :: Xml.Element -> Maybe Frame
toFrame element = Frame <$> src' <*> pure element
  where
    src' =
      element ^. attr "src"
      >>= parseURIReference . unpack . unescapeHtmlEntity


domId :: (HasDom s Xml.Element) => Fold s Text
domId = dom . attr "id" . folded

domClass :: (HasDom s Xml.Element) => Fold s Text
domClass = dom . attr "class" . folded . to words . folded

innerText :: (HasDom s Xml.Element) => Fold s Text
innerText = dom . folding universe . text . to (unwords . words)


unescapeHtmlEntity :: Text -> Text
unescapeHtmlEntity = view $ to (encodeUtf8 . fromStrict) . html . text
