{-# LANGUAGE TemplateHaskell #-}
module Daimust.Data.Attendance
  ( Attendance (..)
  , AttendanceEnter (..)
  , AttendanceLeave (..)
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
  , todParser
  )
where

import ClassyPrelude hiding (many, some, try)

import Control.Lens (Field1 (..), Field2 (..), lens, makeLenses, makePrisms,
                     (^.))
import Data.Time.LocalTime (TimeOfDay (..))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Daimust.Data.Period


-- * Data types

type AttendanceEnter = TimeOfDay
type AttendanceLeave = TimeOfDay

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
  , _enter     :: Maybe AttendanceEnter
  , _leave     :: Maybe AttendanceLeave
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
  tod1 <- todParser
  void $ space *> char '-' <* space
  tod2 <- todParser
  pure (tod1, tod2)

todParser :: Parser TimeOfDay
todParser = try coloned <|> decimal4
  where
    coloned :: Parser TimeOfDay
    coloned = do
      hour' <- L.decimal <?> "hour"
      void $ char ':'
      min' <- L.decimal <?> "minute"
      sec' :: Int <- try (char ':' *> L.decimal) <|> pure 0
      pure $ TimeOfDay hour' min' (fromRational $ toRational sec')

    decimal4 :: Parser TimeOfDay
    decimal4 = do
      hour' :: Text <- pack <$> count 2 digitChar
      let Just hour'' = readMay hour'
      min' :: Text <- pack <$> count 2 digitChar
      let Just min'' = readMay min'
      pure $ TimeOfDay hour'' min'' 0
