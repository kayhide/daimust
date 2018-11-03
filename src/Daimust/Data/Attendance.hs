{-# LANGUAGE TemplateHaskell #-}
module Daimust.Data.Attendance
  ( Attendance (..)
  , AttendanceEnter
  , AttendanceLeave
  , AttendancePeriod
  , period
  , date
  , day
  , dow
  , enter
  , leave
  , noteValue
  , noteLabel
  , color
  , formatAttendance
  , parseHours
  , periodP
  )
where

import           ClassyPrelude        hiding (many, some)

import           Control.Lens         (makeLenses)
import           Formatting           (bprint, center, later, left, sformat,
                                       stext, (%), (%.))
import           Text.Megaparsec
import           Text.Megaparsec.Char


-- * Data types

type AttendanceEnter = Text
type AttendanceLeave = Text

type AttendancePeriod = (Text, Text)

data Attendance =
  Attendance
  { _period    :: AttendancePeriod
  , _date      :: Text
  , _day       :: Text
  , _dow       :: Text
  , _enter     :: AttendanceEnter
  , _leave     :: AttendanceLeave
  , _noteValue :: Text
  , _noteLabel :: Text
  , _color     :: Text
  }
  deriving (Eq, Show)

makeLenses ''Attendance


-- * Helper functions

-- | Format attendence data into @Text@.

formatAttendance :: Attendance -> Text
formatAttendance (Attendance _ _ day' dow' enter' leave' noteValue' noteLabel' _) =
  sformat
  ( (left 4 ' ' %. stext) %
    (left 4 ' ' %. stext) %
    " " % (center 15 ' ' %. (stext % " - " % stext)) %
    " " % stext %
    (later (\s -> bool (bprint (" (" % stext % ")") s) "" $ null s))
  ) day' dow' enter' leave' noteLabel' noteValue'


-- | Parse hours cell @Text@ into enter and leave values.

parseHours :: Text -> Maybe (AttendanceEnter, AttendanceLeave)
parseHours hours = flip (parseMaybe @()) hours $ do
  hour1 <- some digitChar <* char ':'
  min1 <- some digitChar
  void $ space *> char '-' <* space
  hour2 <- some digitChar <* char ':'
  min2 <- some digitChar
  pure (pack (hour1 <> min1), pack (hour2 <> min2))

-- | Text parser of @AttendancePeriod@.

periodP :: Parsec () Text AttendancePeriod
periodP = do
  year <- pack <$> count 4 digitChar
  month <- pack <$> count 2 digitChar
  pure (year, month)
