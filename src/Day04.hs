module Day04 (day04) where

import Common
import Data.List (group, sort)

day04 :: AOCSolution
day04 input = run <$> [id, map sort] <*> pure input

run :: ([String] -> [String]) -> String -> String
run fn = show . countTrue id . map go . lines
  where
    go :: String -> Bool
    go = all ((== 1) . length) . group . sort . fn . words