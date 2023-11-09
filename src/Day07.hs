module Day07 (day07) where

import Common
import Control.Monad.Writer (MonadWriter (tell), execWriter, zipWithM_)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (group, maximumBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Text.Parsec qualified as P

type Programs = M.HashMap String (Int, S.HashSet String)

day07 :: AOCSolution
day07 input = [p1, show p2]
  where
    i = parseInput input
    p1 = part1 i
    p2 = part2 i p1

part1 :: Programs -> String
part1 m
  | M.size m == 1 = head $ M.keys m
  | otherwise = part1 m'
  where
    f = S.fromList $ M.keys $ M.filter (null . snd) m
    m' = M.map (second (`S.difference` f)) $ M.filter (not . null . snd) m

part2 :: Programs -> String -> Int
part2 i r = head <$> execWriter $ weighTree i r
  where
    mode :: (Eq a) => [a] -> Maybe a
    mode [] = Nothing
    mode x = listToMaybe . maximumBy (comparing length) $ group x
    --
    weighTree :: (Monad m, MonadWriter [Int] m) => Programs -> String -> m Int
    weighTree tree root = do
      let (weight, S.toList -> children) = tree M.! root
      childWeights <- mapM (weighTree tree) children
      let Just targetWeight = mode childWeights
          check child actualWeight =
            if actualWeight == targetWeight
              then pure ()
              else tell [fst (tree M.! child) + targetWeight - actualWeight]
      zipWithM_ check children childWeights
      pure $ weight + sum childWeights

parseInput :: String -> M.HashMap String (Int, S.HashSet String)
parseInput = M.fromList . map parseLine . lines

parseLine :: String -> (String, (Int, S.HashSet String))
parseLine = parse' p id
  where
    p = do
      n <- P.many P.letter
      P.char ' '
      P.char '('
      w <- read <$> P.many P.digit
      P.char ')'
      c <- P.choice [P.try q, pure []]
      return (n, (w, S.fromList c))
    q = P.string " -> " *> P.sepBy (P.many P.letter) (P.string ", ")
