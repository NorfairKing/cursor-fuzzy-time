{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyDaySpec where

import Cursor.FuzzyDay
import Cursor.FuzzyDay.Gen ()
import Data.Int
import Data.Time
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  eqSpec @FuzzyDayCursor
  genValidSpec @FuzzyDayCursor
  describe "emptyFuzzyDayCursor" $ do
    it "produces valid cursors" $ producesValid emptyFuzzyDayCursor
    it "makes cursors that makes the guessing produce nothing" $
      forAllValid $ \today ->
        fuzzyDayCursorGuess (emptyFuzzyDayCursor today) `shouldBe` Nothing
  describe "makeFuzzyDayCursor" $ do
    it "produces valid cursors" $ producesValid makeFuzzyDayCursor
    it "makes cursors that makes the guessing produce the given time for recent days" $
      forAll (ModifiedJulianDay . fromIntegral <$> (genValid :: Gen Int16)) $ \d ->
        fuzzyDayCursorGuess (makeFuzzyDayCursor d) `shouldBe` Just d
  describe "rebuildFuzzyDayCursor" $ do
    it "produces valid days" $ producesValid rebuildFuzzyDayCursor
  describe "fuzzyDayCursorTextCursorL" $
    lensSpec fuzzyDayCursorTextCursorL
  describe "fuzzyDayCursorGuess" $
    it "guesses a valid day" $ producesValid fuzzyDayCursorGuess
