{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyDay
    ( FuzzyDayCursor(..)
    , emptyFuzzyDayCursor
    , fuzzyDayCursorTextCursorL
    , fuzzyDayCursorGuess
    ) where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Time

import Text.Megaparsec
import Text.Megaparsec.Char as Char
import Text.Megaparsec.Char.Lexer as Lexer

import Lens.Micro

import Data.FuzzyTime

import Cursor.Text

newtype FuzzyDayCursor = FuzzyDayCursor
    { fuzzyDayCursorTextCursor :: TextCursor
    } deriving (Show, Eq, Generic)

instance Validity FuzzyDayCursor

emptyFuzzyDayCursor :: FuzzyDayCursor
emptyFuzzyDayCursor =
    FuzzyDayCursor {fuzzyDayCursorTextCursor = emptyTextCursor}

fuzzyDayCursorTextCursorL :: Lens' FuzzyDayCursor TextCursor
fuzzyDayCursorTextCursorL =
    lens fuzzyDayCursorTextCursor $ \fdc tc ->
        fdc {fuzzyDayCursorTextCursor = tc}

fuzzyDayCursorGuess :: Day -> FuzzyDayCursor -> Maybe Day
fuzzyDayCursorGuess d FuzzyDayCursor {..} = do
    fd <- parseFuzzyDay $ rebuildTextCursor fuzzyDayCursorTextCursor
    pure $ resolveDay d fd


parseFuzzyDay :: Text -> Maybe FuzzyDay
parseFuzzyDay = parseMaybe fuzzyDayP
