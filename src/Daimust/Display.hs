module Daimust.Display
where

import           ClassyPrelude

import           Control.Lens
import           Data.Default   (Default, def)


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
  traverse_ (print' . second pick') $ zip [0..] table
  where
    pickColumns' :: [Int] -> Row -> Row
    pickColumns' idxs xs = xs ^.. traversed . indices (`elem` idxs)

    print' :: (Int, Row) -> IO ()
    print' ((`elem` headerRows) -> True, row) = putStrLn $ unwords row
    print' ((`elem` dropRows)   -> True, row) = pure ()
    print' (_,                           row) = putStrLn $ intercalate "\t" row
