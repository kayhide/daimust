{-# LANGUAGE TemplateHaskell #-}
module Daimust.Data.Attendance
  ( Attendance (..)
  , AttendanceEnter
  , AttendanceLeave
  , Attendity
  , year
  , month
  , period
  , date
  , day
  , dow
  , enter
  , leave
  , attendity
  , color
  , toAttendity
  , newWorkdayOn
  , newHolidayOn
  , newWorkdayOff
  , formatAttendity
  , parseHours
  )
where

import ClassyPrelude hiding (many, some)

import Control.Lens (Field1 (..), Field2 (..), lens, makeLenses, makePrisms,
                     (^.))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Daimust.Data.Period


-- * Data types

type AttendanceEnter = Text
type AttendanceLeave = Text


data Attendity =
  WorkdayOn Text Text | HolidayOn Text Text | WorkdayOff Text Text
  deriving (Eq, Show)

makePrisms ''Attendity

instance Field1 Attendity Attendity Text Text where
  _1 = lens get' set'
    where
      get' :: Attendity -> Text
      get' (WorkdayOn x _)  = x
      get' (HolidayOn x _)  = x
      get' (WorkdayOff x _) = x

      set' :: Attendity -> Text -> Attendity
      set' (WorkdayOn _ y) x  = WorkdayOn x y
      set' (HolidayOn _ y) x  = HolidayOn x y
      set' (WorkdayOff _ y) x = WorkdayOff x y

instance Field2 Attendity Attendity Text Text where
  _2 = lens get' set'
    where
      get' :: Attendity -> Text
      get' (WorkdayOn _ y)  = y
      get' (HolidayOn _ y)  = y
      get' (WorkdayOff _ y) = y

      set' :: Attendity -> Text -> Attendity
      set' (WorkdayOn x _) y  = WorkdayOn x y
      set' (HolidayOn x _) y  = HolidayOn x y
      set' (WorkdayOff x _) y = WorkdayOff x y


data Attendance =
  Attendance
  { _period    :: Period
  , _date      :: Text
  , _day       :: Text
  , _dow       :: Text
  , _enter     :: AttendanceEnter
  , _leave     :: AttendanceLeave
  , _attendity :: Maybe Attendity
  , _color     :: Text
  }
  deriving (Eq, Show)

makeLenses ''Attendance


-- * Helper functions

-- | Format @Attendity@.

toAttendity :: Text -> Maybe Text -> Maybe Attendity
toAttendity x@"00" y = Just $ WorkdayOn x $ fromMaybe "" y
toAttendity x@"20" y = Just $ HolidayOn x $ fromMaybe "" y
toAttendity x@"21" y = Just $ WorkdayOff x $ fromMaybe "" y
toAttendity _ _      = Nothing

newWorkdayOn :: Attendity
newWorkdayOn = WorkdayOn "00" ""

newHolidayOn :: Attendity
newHolidayOn = HolidayOn "20" ""

newWorkdayOff :: Attendity
newWorkdayOff = WorkdayOn "21" ""

-- | Format @Attendity@.

formatAttendity :: Attendity -> Text
formatAttendity x = x ^. _1 <> " (" <> x ^. _2 <> ")"

-- | Parse hours cell @Text@ into enter and leave values.

parseHours :: Text -> Maybe (AttendanceEnter, AttendanceLeave)
parseHours = parseMaybe hoursParser

type Parser = Parsec Void Text

hoursParser :: Parser (AttendanceEnter, AttendanceLeave)
hoursParser = do
  hour1 <- some digitChar <* char ':'
  min1 <- some digitChar
  void $ space *> char '-' <* space
  hour2 <- some digitChar <* char ':'
  min2 <- some digitChar
  pure (pack (hour1 <> min1), pack (hour2 <> min2))
