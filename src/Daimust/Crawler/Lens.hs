module Daimust.Crawler.Lens
where

import           ClassyPrelude

import           Control.Lens                (Fold, filtered, folded, folding, Prism', prism,
                                              ix, only, to, universe, view, _Just,
                                              (^.), (^?), (^..))
import qualified Data.Map                    as Map
import           Network.URI
import           Text.Xml.Lens               (attr, attributed, html, name,
                                              named, text)

import           Daimust.Crawler.DomSelector
import           Daimust.Crawler.Type


_URI :: Prism' Text URI
_URI = prism tshow $ \s -> maybe (Left s) Right (parseURIReference $ unpack s)


selected :: DomSelector -> Fold Dom Dom
selected (DomSelector factors) = folding universe . filtered' factors
  where
    filtered' [] = filtered (const True)
    filtered' (DomName x : xs) = named (only name') . filtered' xs
      where name' = fromString $ unpack x
    filtered' (DomId x : xs) = attributed (ix "id" . only x) . filtered' xs
    filtered' (DomClass x : xs) = attributed (ix "class" . to words . folded . only x) . filtered' xs


forms :: Fold Dom Form
forms = selected "form" . _Form

_Form :: Prism' Dom Form
_Form = prism (^. dom) $ \s -> maybe (Left s) Right (toForm s)

toForm :: Dom -> Maybe Form
toForm element = do
  guard $ element ^. name == "form"
  pure $ Form (fromMaybe emptyUrl action') fields' element
  where
    action' = element ^? attr "action" . _Just . to unescapeHtmlEntity . _URI

    fields' = Map.fromList $ inputs' <> options'
    inputs' = element ^.. inputs . to (view key &&& view value)

    selects' = element ^.. selected "select" . attributed (ix "name")
    options' = do
      e <- selects'
      maybe [] pure $ do
        k <- e ^. attr "name"
        v <- e ^. selected "option" . attributed (ix "selected") . attr "value"
        pure (k, v)

    emptyUrl = URI "" Nothing "" "" ""


inputs :: Fold Dom Input
inputs = selected "input" . _Input

_Input :: Prism' Dom Input
_Input = prism (^. dom) $ \s -> maybe (Left s) Right (toInput s)

toInput :: Dom -> Maybe Input
toInput element = do
  name' <- element ^. attr "name"
  pure $ Input name' value' element
  where
    value' = element ^. attr "value" . folded


links :: Fold Dom Link
links = selected "a" . _Link

_Link :: Prism' Dom Link
_Link = prism (^. dom) $ \s -> maybe (Left s) Right (toLink s)

toLink :: Dom -> Maybe Link
toLink element = do
  href' <- element ^? attr "href" . _Just . to unescapeHtmlEntity . _URI
  pure $ Link href' element


frames :: Fold Dom Frame
frames = selected "frame" . _Frame

_Frame :: Prism' Dom Frame
_Frame = prism (^. dom) $ \s -> maybe (Left s) Right (toFrame s)

toFrame :: Dom -> Maybe Frame
toFrame element = do
  src' <- element ^? attr "src" . _Just . to unescapeHtmlEntity . _URI
  pure $ Frame src' element


domId :: (HasDom s Dom) => Fold s Text
domId = dom . attr "id" . folded

domClass :: (HasDom s Dom) => Fold s Text
domClass = dom . attr "class" . folded . to words . folded

innerText :: (HasDom s Dom) => Fold s Text
innerText = dom . folding universe . text . to (unwords . words)


unescapeHtmlEntity :: Text -> Text
unescapeHtmlEntity = view $ to (encodeUtf8 . fromStrict) . html . text
