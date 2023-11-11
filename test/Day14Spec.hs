module Day14Spec (spec) where

import SpecHelper

testInput = "flqrgnkx"

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` ["8108", "1212"]

  it "Actual" $ do
    withFile
      "inputs/day14.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day14 actualInput `shouldBe` ["8074", "1212"]
      )