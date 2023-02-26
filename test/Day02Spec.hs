module Day02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 2" $ do
  it "Actual" $ do
    withFile
      "inputs/day02.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day02 actualInput `shouldBe` ["45972", "326"]
      )