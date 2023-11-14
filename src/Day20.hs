{-# OPTIONS_GHC -Wno-x-partial #-}

module Day20 (day20) where

import Common
import Data.Bifunctor (second)
import Data.Function(on)
import Data.HashMap.Strict qualified as M
import Data.List (minimumBy)
import Text.Parsec qualified as P

day20 :: AOCSolution
day20 input = show <$> ([part1, part2] <*> pure i)
  where
    i = parseInput input

part1 :: [(Int, [Point3d])] -> Int
part1 = fst . minimumBy (compare `on` snd) . map (\(n, x:_) -> (n, distance x)) . go id 1000
  
part2 :: [(Int, [Point3d])] -> Int
part2 = length . go removeDuplicates 1000

go :: ([(Int, [Point3d])] -> [(Int, [Point3d])]) -> Int -> [(Int, [Point3d])] -> [(Int, [Point3d])]
go _ 0 i = i
go f n i = go f (pred n) i'
  where
    i' = f $ map (second simulate) i

removeDuplicates :: [(Int, [Point3d])] -> [(Int, [Point3d])]
removeDuplicates i = filter (\(_, x:_) -> x `M.member` ps) i
  where
    ps = M.filter (==1) $
      foldr (flip (M.insertWith (+)) 1) M.empty $
      map (head . snd) i

parseInput :: String -> [(Int, [Point3d])]
parseInput = zip [0..] . map (parse' p id) . lines
  where
    p = q `P.sepBy` P.string ", "
    q = do 
      P.letter
      P.string "=<"
      n <- tuplify3 <$> number `P.sepBy` P.char ','
      P.char '>'
      pure n

simulate :: [Point3d] -> [Point3d]
simulate [x, y, z] = [x', y', z]
  where
    y' = addPoint3d y z
    x' = addPoint3d x y'

distance :: Point3d -> Int
distance = sum . map abs . untuplify3

addPoint3d :: Point3d -> Point3d -> Point3d
addPoint3d (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
