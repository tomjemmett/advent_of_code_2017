module Day09 (day09) where

import Common

day09 :: AOCSolution
day09 = f 0 0 0
  where
    f :: Int -> Int -> Int -> String -> [String]
    f lvl p1 p2 [] = show <$> [p1, p2]
    f lvl p1 p2 (x : xs) = case x of
      '!' -> f lvl p1 p2 $ tail xs
      '<' -> f lvl p1 d' xs'
      '{' -> f (lvl + 1) (p1 + lvl + 1) p2 xs
      '}' -> f (lvl - 1) p1 p2 xs
      _ -> f lvl p1 p2 xs
      where
        (d', xs') = g p2 xs
    --
    g :: Int -> String -> (Int, String)
    g p2 (x : xs) = case x of
      '!' -> g p2 $ tail xs
      '>' -> (p2, xs)
      _ -> g (p2 + 1) xs
