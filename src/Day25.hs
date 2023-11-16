{-# OPTIONS_GHC -Wno-x-partial #-}

module Day25 where -- (day25) where

import Common
import Data.Char (digitToInt)
import Data.List (splitAt)
import Data.HashMap.Strict qualified as M

type Program = M.HashMap Char [(Int, Int, Char)]
type Tape = M.HashMap Int Int

day25 :: AOCSolution
day25 input = show <$> [p1]
  where
    (s, d, i) = parseInput input
    p1 = go i s 0 d M.empty

go :: Program -> Char -> Int -> Int -> Tape -> Int
go _ _ _ 0 t = sum $ M.elems t
go i s p d t = go i s' p' d' t'
  where
    v = M.lookupDefault 0 p t
    (a, b, s') = (i M.! s) !! v
    t' = M.insert p a t
    p' = p + b
    d' = pred d


parseInput :: String -> (Char, Int, Program)
parseInput input = (i, d, s)
  where
    (initState : diagnostic : states) = lines input
    i = initState !! 15
    d = read $ (words diagnostic) !! 5
    s = M.fromList $ parseStates states

parseStates :: [String] -> [(Char, [(Int, Int, Char)])]
parseStates [] = []
parseStates xs = parseState x : parseStates xs'
  where
    (x, xs') = splitAt 10 xs

parseState :: [String] -> (Char, [(Int, Int, Char)])
parseState (_ : s : sb) = (s !! 9, [f a, f b])
  where
    (a, b) = splitAt 4 sb
    f [_, x, y, z] = (px x, py y, pz z)
    px :: String -> Int
    px (drop 22 -> x : _) = digitToInt x
    py :: String -> Int
    py = \case
      "    - Move one slot to the right." -> 1
      _ -> -1
    pz :: String -> Char
    pz (drop 26 -> z : _) = z

getI = parseInput <$> readFile "inputs/day25.txt"