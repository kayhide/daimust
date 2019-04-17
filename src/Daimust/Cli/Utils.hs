module Daimust.Cli.Utils where

import ClassyPrelude

import Data.List.Split (chunksOf)
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (Color (..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import Formatting (center, left, sformat, stext, (%), (%.))
import Formatting.Time (hm, dayOfMonthS)

import Daimust.Data.Attendance


-- | Pretty prints @Attendance@.

printAttendance :: MonadIO m => Attendance -> m ()
printAttendance (Attendance _ date' dow' enter' leave' attendity' color') = do
  let doc = pretty dayCol <+> pretty timeCol <+> Pretty.softline' <+> pretty noteCol
  let annotate' =
        case chunksOf 2 . unpack $ drop 1 color' of
          ["ff", "ff", "ff"] -> id
          ["ff", "ff", _]    -> Pretty.annotate $ Pretty.color Yellow
          ["ff", _, "ff"]    -> Pretty.annotate $ Pretty.color Magenta
          [_, "ff", "ff"]    -> Pretty.annotate $ Pretty.color Cyan
          ["ff", _, _]       -> Pretty.annotate $ Pretty.color Red
          [_, "ff", _]       -> Pretty.annotate $ Pretty.color Green
          [_, _, "ff"]       -> Pretty.annotate $ Pretty.color Blue
          _                  -> id
  liftIO $ Pretty.putDoc $ Pretty.indent 2 (annotate' doc) <+> Pretty.line
  where
    dayCol = sformat ((left 3 ' ' %. dayOfMonthS) % (left 3 ' ' %. stext)) date' dow'
    timeCol = case (enter', leave') of
      (Just enter'', Just leave'') ->
        sformat (center 15 ' ' %. (hm % " - " % hm)) enter'' leave''
      _ ->
        replicate 15 ' '
    noteCol = maybe "" formatAttendity attendity'
