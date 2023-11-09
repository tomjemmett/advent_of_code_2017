module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  it "Actual" $ do
    withFile
      "inputs/day09.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day09 actualInput `shouldBe` ["14212", "6569"]
      )