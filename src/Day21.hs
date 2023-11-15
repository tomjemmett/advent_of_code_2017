{-# OPTIONS_GHC -Wno-x-partial #-}

module Day21 (day21) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)

type Image = [String]
type Enhancer = Image -> Image

day21 :: AOCSolution
day21 input = show . go <$> [5, 18]
  where
    i = parseInput input
    r = iterate (enhance i) [".#.", "..#", "###"]
    go :: Int -> Int
    go n = countTrue (=='#') . concat $ r !! n

enhance :: Enhancer -> Image -> Image
enhance e img = map concat . transpose . map e . transpose . map chunk =<< chunk img
  where
    chunk = chunksOf (if even (length img) then 2 else 3)

parseInput :: String -> Enhancer
parseInput input = (e M.!)
  where
    e = M.fromList . concatMap f . map2 (splitOn "/") . map (splitOn " => ") . lines $ input
    f [x, n] = map (,n) $ symmetries x
    symmetries :: Image -> [Image]
    symmetries i = take 4 . iterate rotate =<< [i, reverse i]
      where
        rotate :: [String] -> [String]
        rotate = reverse . transpose
