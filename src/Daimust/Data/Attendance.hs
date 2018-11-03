module Daimust.Data.Attendance
  ( Attendance (..)
  , AttendancePeriod
  , formatAttendance
  , parseHours
  )
where

import           ClassyPrelude        hiding (many, some)

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
  { period    :: AttendancePeriod
  , date      :: Text
  , day       :: Text
  , dow       :: Text
  , enter     :: AttendanceEnter
  , leave     :: AttendanceLeave
  , noteValue :: Text
  , noteLabel :: Text
  , color     :: Text
  }
  deriving (Eq, Show)


-- * Helper functions

-- | Format attendence data into @Text@.

formatAttendance :: Attendance -> Text
formatAttendance Attendance {..} =
  sformat
  ( (left 4 ' ' %. stext) %
    (left 4 ' ' %. stext) %
    " " % (center 15 ' ' %. (stext % " - " % stext)) %
    " " % stext %
    (later (\s -> bool (bprint (" (" % stext % ")") s) "" $ null s))
  ) day dow enter leave noteLabel noteValue


-- | Parse hours cell @Text@ into enter and leave values.

parseHours :: Text -> Maybe (AttendanceEnter, AttendanceLeave)
parseHours hours = flip (parseMaybe @()) hours $ do
  hour1 <- some digitChar <* char ':'
  min1 <- some digitChar
  void $ space *> char '-' <* space
  hour2 <- some digitChar <* char ':'
  min2 <- some digitChar
  pure (pack (hour1 <> min1), pack (hour2 <> min2))
