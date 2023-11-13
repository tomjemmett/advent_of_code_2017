{-# OPTIONS_GHC -Wno-x-partial #-}

module Day05 (day05) where

import Common
import Control.Monad.ST (runST)
import Data.HashMap.Strict qualified as H
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV

day05 :: AOCSolution
day05 input = show . go v <$> [succ, p2]
  where
    v = V.fromList . map read . lines $ input
    p2 v = (if v >= 3 then pred else succ) v

go :: V.Vector Int -> (Int -> Int) -> Int
go v fn = runST $ V.thaw v >>= step 0 0
  where
    step n i vm
      | i < 0 || i >= MV.length vm = return n
      | otherwise = do
          val <- MV.read vm i
          MV.write vm i $ fn val
          step (succ n) (i + val) vm
