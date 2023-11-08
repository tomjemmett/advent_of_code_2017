module Day05Spec (spec) where

import SpecHelper

testInput = "0\n3\n0\n1\n-3"

spec :: Spec
spec = describe "Day 5" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` ["5", "10"]

  it "Actual" $ do
    withFile
      "inputs/day05.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day05 actualInput `shouldBe` ["372671", "25608480"]
      )