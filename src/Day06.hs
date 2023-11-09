module Day06 (day06) where

import Common
import Data.HashMap.Strict qualified as M

day06 :: AOCSolution
day06 = go M.empty 0 . wordSeparatedInts

go :: M.HashMap [Int] Int -> Int -> [Int] -> [String]
go h n b =
  case M.lookup b h of
    Just j -> show <$> [n, n - j]
    Nothing -> go (M.insert b n h) (succ n) nb
      where
        nb = f $ span (maximum b /=) b
        --
        f :: ([Int], [Int]) -> [Int]
        f (x, y : z) = g y (0 : reverse x) z
        --
        g :: Int -> [Int] -> [Int] -> [Int]
        g 0 x y = reverse x ++ y
        g i x [] = g i [] (reverse x)
        g i x (y : ys) = g (i - 1) ((y + 1) : x) ys