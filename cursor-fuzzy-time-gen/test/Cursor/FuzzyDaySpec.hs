{-# LANGUAGE TypeApplications #-}

module Cursor.FuzzyDaySpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Cursor.FuzzyDay
import Cursor.FuzzyDay.Gen ()

spec :: Spec
spec = do
    eqSpecOnValid @FuzzyDayCursor
    genValidSpec @FuzzyDayCursor
    describe "emptyFuzzyDayCursor" $
        it "is valid" $ shouldBeValid emptyFuzzyDayCursor
    describe "fuzzyDayCursorGuess" $
        it "guesses a valid day" $ producesValidsOnValids2 fuzzyDayCursorGuess
    describe "fuzzyDayCursorTextCursorL" $
        lensSpecOnValid fuzzyDayCursorTextCursorL
