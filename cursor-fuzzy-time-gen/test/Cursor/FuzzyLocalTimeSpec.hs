{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyLocalTimeSpec where

import Data.Int
import Data.Time

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

import Cursor.FuzzyLocalTime
import Cursor.FuzzyLocalTime.Gen ()
import Data.FuzzyTime.Types

spec :: Spec
spec = do
  eqSpecOnValid @FuzzyLocalTimeCursor
  genValidSpec @FuzzyLocalTimeCursor
  describe "emptyFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids emptyFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyLocalTimeCursorGuess (emptyFuzzyLocalTimeCursor today) `shouldBe`
        Nothing
  describe "makeFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $
      producesValidsOnValids makeFuzzyLocalTimeCursor
    it
      "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyLocalTimeCursorGuess
          (makeFuzzyLocalTimeCursor (OnlyDaySpecified d)) `shouldBe`
        Just (OnlyDaySpecified d)
  describe "rebuildFuzzyLocalTimeCursor" $ do
    it "produces valid time of day" $
      producesValidsOnValids rebuildFuzzyLocalTimeCursor
  describe "fuzzyLocalTimeCursorTextCursorL" $
    lensSpecOnValid fuzzyLocalTimeCursorTextCursorL
  describe "fuzzyLocalTimeCursorGuess" $
    it "guesses a valid time of day" $
    producesValidsOnValids fuzzyLocalTimeCursorGuess
