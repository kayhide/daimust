module Daimust.Crawler.DomSelector
where

import           ClassyPrelude        hiding (many, some)

import           Text.Megaparsec
import           Text.Megaparsec.Char


data DomFactor = DomName Text | DomId Text | DomClass Text
  deriving (Show)

newtype DomSelector = DomSelector [DomFactor]

instance IsString DomSelector where
  fromString = parseSelector . pack

type Parser = Parsec () Text

parseSelector :: Text -> DomSelector
parseSelector = DomSelector . fromMaybe [] . parseMaybe selector'
  where
    selector' =  many (name' <|> id' <|> class') :: Parser [DomFactor]
    chunk' = some $ noneOf (['#', '.'] :: [Char])
    name' = DomName . pack <$> chunk'
    id' = DomId . pack <$> (char '#' >> chunk')
    class' = DomClass . pack <$> (char '.' >> chunk')
