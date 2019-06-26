module Daimust.Crawler.Lens
where

import ClassyPrelude

import Control.Lens (Fold, Prism', filtered, folded, folding, ix, only, prism,
                     to, universe, view, (^.), (^..), (^?), _Just)
import qualified Data.Map as Map
import Network.URI
import Text.HTML.DOM (parseLBS)
import qualified Text.XML as Xml
import Text.XML.Lens (attribute, attributeIs, attributeSatisfies, ell, localName, root, text)

import Daimust.Crawler.DomSelector
import Daimust.Crawler.Type


_URI :: Prism' Text URI
_URI = prism tshow $ \s -> maybe (Left s) Right (parseURIReference $ unpack s)


selected :: DomSelector -> Fold Dom Dom
selected (DomSelector factors) = folding universe . filtered' factors
  where
    filtered' :: [DomFactor] -> Fold Dom Dom
    filtered' [] = filtered (const True)
    filtered' (DomName x : xs) = ell x . filtered' xs
    filtered' (DomId x : xs) = attributeIs "id" x . filtered' xs
    filtered' (DomClass x : xs) = attributeSatisfies "class" (\att -> x `elem` words att) . filtered' xs


forms :: Fold Dom Form
forms = selected "form" . _Form

_Form :: Prism' Dom Form
_Form = prism (^. dom) $ \s -> maybe (Left s) Right (toForm s)

toForm :: Dom -> Maybe Form
toForm element = do
  guard $ element ^. localName == "form"
  pure $ Form (fromMaybe emptyUrl action') fields' element
  where
    action' = element ^? attribute "action" . _Just . to unescapeHtmlEntity . _URI

    fields' = Map.fromList $ inputs' <> options'
    inputs' = element ^.. inputs . to (view key &&& view value)

    selects' = element ^.. selected "select" . attributeSatisfies "name" (const True)
    options' = do
      e <- selects'
      maybe [] pure $ do
        k <- e ^. attribute "name"
        v <- e ^. selected "option" . attributeSatisfies "selected" (const True) . attribute "value"
        pure (k, v)

    emptyUrl = URI "" Nothing "" "" ""


inputs :: Fold Dom Input
inputs = selected "input" . _Input

_Input :: Prism' Dom Input
_Input = prism (^. dom) $ \s -> maybe (Left s) Right (toInput s)

toInput :: Dom -> Maybe Input
toInput element = do
  name' <- element ^. attribute "name"
  pure $ Input name' value' element
  where
    value' = element ^. attribute "value" . folded


links :: Fold Dom Link
links = selected "a" . _Link

_Link :: Prism' Dom Link
_Link = prism (^. dom) $ \s -> maybe (Left s) Right (toLink s)

toLink :: Dom -> Maybe Link
toLink element = do
  href' <- element ^? attribute "href" . _Just . to unescapeHtmlEntity . _URI
  pure $ Link href' element


frames :: Fold Dom Frame
frames = selected "frame" . _Frame

_Frame :: Prism' Dom Frame
_Frame = prism (^. dom) $ \s -> maybe (Left s) Right (toFrame s)

toFrame :: Dom -> Maybe Frame
toFrame element = do
  src' <- element ^? attribute "src" . _Just . to unescapeHtmlEntity . _URI
  pure $ Frame src' element


domId :: (HasDom s Dom) => Fold s Text
domId = dom . attribute "id" . folded

domClass :: (HasDom s Dom) => Fold s Text
domClass = dom . attribute "class" . folded . to words . folded

innerText :: (HasDom s Dom) => Fold s Text
innerText = dom . folding universe . text . to (unwords . words)


unescapeHtmlEntity :: Text -> Text
unescapeHtmlEntity = view $ to (encodeUtf8 . fromStrict) . html . text

html :: Fold LByteString Xml.Element
html = to parseLBS . root
