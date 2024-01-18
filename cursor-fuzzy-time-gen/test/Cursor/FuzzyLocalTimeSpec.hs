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
        fuzzyLocalTimeCursorGuessForwards (emptyFuzzyLocalTimeCursor today)
          `shouldBe` Nothing
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyLocalTimeCursorGuessBackwards (emptyFuzzyLocalTimeCursor today)
          `shouldBe` Nothing

  describe "makeFuzzyLocalTimeCursor" $ do
    it "produces valid cursors" $
      producesValid makeFuzzyLocalTimeCursor
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyLocalTimeCursorGuessForwards
          (makeFuzzyLocalTimeCursor (OnlyDaySpecified d))
          `shouldBe` Just (OnlyDaySpecified d)
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyLocalTimeCursorGuessBackwards
          (makeFuzzyLocalTimeCursor (OnlyDaySpecified d))
          `shouldBe` Just (OnlyDaySpecified d)

  describe "rebuildFuzzyLocalTimeCursorForwards" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyLocalTimeCursorForwards
  describe "rebuildFuzzyLocalTimeCursorBackwards" $ do
    it "produces valid time of day" $
      producesValid rebuildFuzzyLocalTimeCursorBackwards

  describe "fuzzyLocalTimeCursorTextCursorL" $
    lensSpec fuzzyLocalTimeCursorTextCursorL

  describe "fuzzyLocalTimeCursorGuessForwards" $
    it "guesses a valid time of day" $
      producesValid fuzzyLocalTimeCursorGuessForwards
  describe "fuzzyLocalTimeCursorGuessBackwards" $
    it "guesses a valid time of day" $
      producesValid fuzzyLocalTimeCursorGuessBackwards
