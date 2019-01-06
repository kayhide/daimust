{-# LANGUAGE TemplateHaskell #-}
module Daimust.Data.Attendance
  ( Attendance (..)
  , AttendanceEnter
  , AttendanceLeave
, Period (..)
  , year
  , month
  , period
  , date
  , day
  , dow
  , enter
  , leave
  , noteValue
  , noteLabel
  , color
  , printPeriod
  , formatAttendance
  , printAttendance
  , parseHours
  , periodP
  )
where

import           ClassyPrelude                             hiding (many, some)

import           Control.Lens                              (makeLenses)
import           Data.List.Split                           (chunksOf)
import           Data.Text.Prettyprint.Doc                 (pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import           Data.Text.Prettyprint.Doc.Render.Terminal (Color (..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Data.Void                                 (Void)
import           Formatting                                (bprint, center,
                                                            fprint, int, later,
                                                            left, sformat,
                                                            stext, (%), (%.))
import           Text.Megaparsec
import           Text.Megaparsec.Char


-- * Data types

type AttendanceEnter = Text
type AttendanceLeave = Text

data Period =
  Period
  { _year  :: Int
  , _month :: Int
  }
  deriving (Eq, Show, Read)

makeLenses ''Period

data Attendance =
  Attendance
  { _period    :: Period
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

-- | Print @Period@.

printPeriod :: Period -> IO ()
printPeriod (Period year' month') = do
  fprint ((left 4 '0' %. int) % "-" % (left 2 '0' %. int)) year' month'
  putStrLn ""

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


-- | Pretty prints @Attendance@.

printAttendance :: Attendance -> IO ()
printAttendance (Attendance _ _ day' dow' enter' leave' noteValue' noteLabel' color') = do
  let doc = pretty dayCol <+> pretty timeCol <+> Pretty.softline' <+> pretty noteCol
  let annotate' =
        case (chunksOf 2 . unpack $ drop 1 $ color') of
          ["ff", "ff", "ff"] -> id
          ["ff", "ff", _]    -> Pretty.annotate $ Pretty.color Yellow
          ["ff", _, "ff"]    -> Pretty.annotate $ Pretty.color Magenta
          [_, "ff", "ff"]    -> Pretty.annotate $ Pretty.color Cyan
          ["ff", _, _]       -> Pretty.annotate $ Pretty.color Red
          [_, "ff", _]       -> Pretty.annotate $ Pretty.color Green
          [_, _, "ff"]       -> Pretty.annotate $ Pretty.color Blue
          _                  -> id
  Pretty.putDoc $ Pretty.indent 2 (annotate' doc) <+> Pretty.line
  where
    dayCol = sformat ((left 3 ' ' %. stext) % (left 3 ' ' %. stext)) day' dow'
    timeCol = sformat ((center 13 ' ' %. (stext % " - " % stext))) enter' leave'
    noteCol = bool (sformat (stext % " (" % stext % ")") noteLabel' noteValue') "" $ null noteValue'


-- | Parse hours cell @Text@ into enter and leave values.

parseHours :: Text -> Maybe (AttendanceEnter, AttendanceLeave)
parseHours hours = flip (parseMaybe @()) hours $ do
  hour1 <- some digitChar <* char ':'
  min1 <- some digitChar
  void $ space *> char '-' <* space
  hour2 <- some digitChar <* char ':'
  min2 <- some digitChar
  pure (pack (hour1 <> min1), pack (hour2 <> min2))


-- | Text parser of @Period@.

type Parser = Parsec Void Text

periodP :: Parser Period
periodP = do
  year' <- readMay <$> count 4 digitChar
  month' <- readMay <$> count 2 digitChar
  case (Period <$> year' <*> month') of
    Just period' -> pure period'
    _            -> fail "internal error: failed to convert digitChar to Int"
