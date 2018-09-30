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
    describe "emptyFuzzyDayCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids emptyFuzzyDayCursor
        it "makes cursors that makes the guessing produce nothing" $
            forAllValid $ \today ->
                fuzzyDayCursorGuess (emptyFuzzyDayCursor today) `shouldBe`
                Nothing
    describe "makeFuzzyDayCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids makeFuzzyDayCursor
        it "makes cursors that makes the guessing produce the given time" $
            forAllValid $ \d ->
                fuzzyDayCursorGuess (makeFuzzyDayCursor d) `shouldBe` Just d
    describe "rebuildFuzzyDayCursor" $ do
        it "produces valid days" $ producesValidsOnValids rebuildFuzzyDayCursor
    describe "fuzzyDayCursorTextCursorL" $
        lensSpecOnValid fuzzyDayCursorTextCursorL
    describe "fuzzyDayCursorGuess" $
        it "guesses a valid day" $ producesValidsOnValids fuzzyDayCursorGuess
