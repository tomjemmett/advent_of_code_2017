module Day01 (day01) where

import Common
import Data.Char (digitToInt)

day01 :: AOCSolution
day01 input = go <$> [1, length input `div` 2] <*> pure (head $ lines input)

go :: Int -> String -> String
go n i = show . sum . map (digitToInt . fst) . filter (uncurry (==)) $ zip i $ drop n $ cycle i