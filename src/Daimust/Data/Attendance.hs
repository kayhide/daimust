module Daimust.Data.Attendance
  ( Attendance (..)
  , parseHours
  )
where

import           ClassyPrelude        hiding (many, some)

import           Text.Megaparsec
import           Text.Megaparsec.Char


-- * Data types

type AttendanceEnter = Text
type AttendanceLeave = Text

data Attendance =
  Attendance
  { date      :: Text
  , day       :: Text
  , dow       :: Text
  , enter     :: AttendanceEnter
  , leave     :: AttendanceLeave
  , noteValue :: Text
  , noteLabel :: Text
  }
  deriving (Eq, Show)


-- * Helper functions

parseHours :: Text -> Maybe (AttendanceEnter, AttendanceLeave)
parseHours hours = flip (parseMaybe @()) hours $ do
  hour1 <- some digitChar <* char ':'
  min1 <- some digitChar
  void $ space *> char '-' <* space
  hour2 <- some digitChar <* char ':'
  min2 <- some digitChar
  pure (pack (hour1 <> min1), pack (hour2 <> min2))
