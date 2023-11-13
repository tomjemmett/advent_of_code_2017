module Day19 (day19) where

import Common
import Data.HashMap.Strict qualified as M

day19 :: AOCSolution
day19 input = go i 0 start (1, 0) ""
  where
    i = parseInput input
    start = head $ filter ((==0) . fst) $ M.keys i

go :: M.HashMap Point2d Char -> Int -> Point2d -> Point2d -> String -> [String]
go i n p@(px, py) d@(dx, dy) r
  | not $ p `M.member` i = [reverse r, show n]
  | v == '+' = changeDir
  | v == '-' = next r
  | v == '|' = next r
  | otherwise = next (v:r)
  where
    v = i M.! p
    p' = (px + dx, py + dy)
    next = go i (succ n) p' d

    pl = (px, py-1)
    pr = (px, py+1)
    pd = (px+1, py)
    pu = (px-1, py)

    changeDir
      | abs dx == 1 = (flip $ uncurry $ go i $ succ n) r $ if pl `M.member` i then (pl, (0, -1)) else (pr, (0, 1))
      | abs dy == 1 = (flip $ uncurry $ go i $ succ n) r $ if pu `M.member` i then (pu, (-1, 0)) else (pd, (1, 0))

parseInput :: String -> M.HashMap Point2d Char
parseInput i = M.fromList [((r, c), v)
                          | (r, line) <- zip [0..] $ lines i
                          , (c, v) <- zip [0..] line
                          , v /= ' ']
