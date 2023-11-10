module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  it "Actual" $ do
    withFile
      "inputs/day11.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day11 actualInput `shouldBe` ["685", "1457"]
      )