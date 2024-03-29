module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  it "Actual" $ do
    withFile
      "inputs/day10.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day10 actualInput `shouldBe` ["11375", "e0387e2ad112b7c2ef344e44885fe4d8"]
      )