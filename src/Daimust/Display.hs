module Daimust.Display
where

import ClassyPrelude

import Control.Lens
import Data.Default (Default, def)


type Row = [Text]

data DisplayTableConfig =
  DisplayTableConfig
  { headerRows  :: [Int]
  , dropRows    :: [Int]
  , pickColumns :: Maybe [Int]
  }
  deriving (Eq, Show)

instance Default DisplayTableConfig where
  def = DisplayTableConfig [] [] Nothing


displayTable :: [Row] -> IO ()
displayTable = displayTableWith def


displayTableWith :: DisplayTableConfig -> [Row] -> IO ()
displayTableWith DisplayTableConfig {..} table = do
  let pick' = maybe id pickColumns' pickColumns
  traverse_ (sayShow' . second pick') $ zip [0..] table
  where
    pickColumns' :: [Int] -> Row -> Row
    pickColumns' idxs xs = xs ^.. traversed . indices (`elem` idxs)

    sayShow' :: (Int, Row) -> IO ()
    sayShow' ((`elem` headerRows) -> True, row) = say $ unwords row
    sayShow' ((`elem` dropRows)   -> True, row) = pure ()
    sayShow' (_,                           row) = say $ intercalate "\t" row
