module Day06Spec (spec) where

import SpecHelper

testInput = "0\t2\t7\t0"

spec :: Spec
spec = describe "Day 6" $ do
  it "Sample" $ do
    day06 testInput `shouldBe` ["5", "4"]

  it "Actual" $ do
    withFile
      "inputs/day06.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day06 actualInput `shouldBe` ["6681", "2392"]
      )