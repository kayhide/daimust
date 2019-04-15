{-# LANGUAGE TemplateHaskell #-}
module Daimust.Data.Period
  ( Period (..)
  , year
  , month
  , formatPeriod
  , periodP
  , parsePeriodMaybe
  )
where

import ClassyPrelude hiding (many, some)

import Control.Lens (makeLenses)
import Data.Void (Void)
import Formatting (int, left, sformat, (%), (%.))
import Text.Megaparsec (Parsec, count, many, parseMaybe)
import Text.Megaparsec.Char (char, digitChar)


-- * Data types

data Period =
  Period
  { _year  :: Int
  , _month :: Int
  }
  deriving (Eq, Show, Read)

makeLenses ''Period


-- * Helper functions

-- | Format @Period@.

formatPeriod :: Period -> Text
formatPeriod (Period year' month') =
  sformat ((left 4 '0' %. int) % "-" % (left 2 '0' %. int)) year' month'


-- | Text parser of @Period@.

type Parser = Parsec Void Text

periodP :: Parser Period
periodP = do
  year' <- readMay <$> count 4 digitChar
  void $ many (char '-')
  month' <- readMay <$> count 2 digitChar
  case Period <$> year' <*> month' of
    Just period' -> pure period'
    _            -> fail "internal error: failed to convert digitChar to Int"

parsePeriodMaybe :: Text -> Maybe Period
parsePeriodMaybe = parseMaybe periodP
