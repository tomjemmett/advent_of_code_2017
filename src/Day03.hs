module Day03 (day03) where

import Common
import Data.Bifunctor (Bifunctor, first, second)
import Data.Function ((&))
import Data.HashMap.Strict qualified as H

day03 :: AOCSolution
day03 input = show <$> ([part1, part2] <*> pure (read input))

part1 :: Integer -> Integer
part1 i = (t - 1) - (t * t - i)
  where
    t = toInteger . ceiling . sqrt . fromIntegral $ i

part2 :: Integer -> Integer
part2 i = head $ dropWhile (< i) $ 1 : fn positions init
  where
    init :: H.HashMap Point2d Integer
    init = H.singleton (0, 0) 1
    fn :: [Point2d] -> H.HashMap Point2d Integer -> [Integer]
    fn (p : ps) m = v : fn ps (H.insert p v m)
      where
        v = foldl (\v i -> v + H.lookupDefault 0 i m) 0 . point2dNeighboursDiags $ p
    positions :: [Point2d]
    positions = tail $ scanl (&) (0, 0) $ concat $ zipWith replicate s d
      where
        s :: [Int]
        s = concatMap (\a -> [a, a]) [1 ..]
        d :: [Point2d -> Point2d]
        d =
          cycle
            [ first succ, -- right
              second succ, -- up
              first pred, -- left
              second pred -- right
            ]