{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyLocalTimeSpec where

import Cursor.FuzzyLocalTime
import Cursor.FuzzyLocalTime.Gen ()
import Data.FuzzyTime.Types
import Data.Int
import Data.Time
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  eqSpec @FuzzyLocalTimeCursor
  genValidSpec @FuzzyLocalTimeCursor
  describe "emptyFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $
      producesValid emptyFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyLocalTimeCursorGuess (emptyFuzzyLocalTimeCursor today)
          `shouldBe` Nothing
  describe "makeFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $
      producesValid makeFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyLocalTimeCursorGuess
          (makeFuzzyLocalTimeCursor (OnlyDaySpecified d))
          `shouldBe` Just (OnlyDaySpecified d)
  describe "rebuildFuzzyLocalTimeCursor" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyLocalTimeCursor
  describe "fuzzyLocalTimeCursorTextCursorL" $
    lensSpec fuzzyLocalTimeCursorTextCursorL
  describe "fuzzyLocalTimeCursorGuess" $
    it "guesses a valid time of day" $
      producesValid fuzzyLocalTimeCursorGuess
