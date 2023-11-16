{-# OPTIONS_GHC -Wno-x-partial #-}

module Day24 (day24) where

import Common
import Control.Applicative (Alternative (..))
import Control.Monad.Trans.State (StateT (..), evalStateT)
import Data.Bifunctor (first)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Data.Tuple (swap)

type Comp = (Int, Int)

day24 :: AOCSolution
day24 input = show <$> [p1, p2]
  where
    i = parseInput input
    b = runStateT (bridge 0) i
    p1 = maximum $ fst <$> b
    p2 = snd . maximum $ first (Down . length) . swap <$> b

parseInput :: String -> [Comp]
parseInput = map (tuplify2 . map read . splitOn "/") . lines

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

bridge :: Int -> StateT [Comp] [] Int
bridge frm = do
  (x, y) <- StateT select
  next <-
    if
        | x == frm -> return y
        | y == frm -> return x
        | otherwise -> empty
  rest <- return 0 <|> bridge next
  return $ x + y + rest
