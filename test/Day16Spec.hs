module Day16Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  it "Actual" $ do
    withFile
      "inputs/day16.txt"
      ReadMode
      ( \h -> do
          actualInput <- hGetContents h
          day16 actualInput `shouldBe` ["giadhmkpcnbfjelo", "njfgilbkcoemhpad"]
      )