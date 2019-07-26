{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyTimeOfDay
  ( FuzzyTimeOfDayCursor(..)
  , emptyFuzzyTimeOfDayCursor
  , makeFuzzyTimeOfDayCursor
  , rebuildFuzzyTimeOfDayCursor
  , fuzzyTimeOfDayCursorTextCursorL
  , fuzzyTimeOfDayCursorGuess
  ) where

import GHC.Generics (Generic)

import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Validity

import Text.Megaparsec

import Lens.Micro

import Data.FuzzyTime

import Cursor.Text

data FuzzyTimeOfDayCursor =
  FuzzyTimeOfDayCursor
    { fuzzyTimeOfDayCursorTextCursor :: TextCursor
    , fuzzyTimeOfDayCursorBaseTimeOfDay :: TimeOfDay
    }
  deriving (Show, Eq, Generic)

instance Validity FuzzyTimeOfDayCursor

emptyFuzzyTimeOfDayCursor :: TimeOfDay -> FuzzyTimeOfDayCursor
emptyFuzzyTimeOfDayCursor d =
  FuzzyTimeOfDayCursor
    {fuzzyTimeOfDayCursorTextCursor = emptyTextCursor, fuzzyTimeOfDayCursorBaseTimeOfDay = d}

makeFuzzyTimeOfDayCursor :: TimeOfDay -> FuzzyTimeOfDayCursor
makeFuzzyTimeOfDayCursor d =
  FuzzyTimeOfDayCursor
    { fuzzyTimeOfDayCursorTextCursor =
        fromJust $ makeTextCursor $ T.pack $ formatTime defaultTimeLocale "%T%Q" d
    , fuzzyTimeOfDayCursorBaseTimeOfDay = d
    }

rebuildFuzzyTimeOfDayCursor :: FuzzyTimeOfDayCursor -> TimeOfDay
rebuildFuzzyTimeOfDayCursor fdc@FuzzyTimeOfDayCursor {..} =
  fromMaybe fuzzyTimeOfDayCursorBaseTimeOfDay $ fuzzyTimeOfDayCursorGuess fdc

fuzzyTimeOfDayCursorTextCursorL :: Lens' FuzzyTimeOfDayCursor TextCursor
fuzzyTimeOfDayCursorTextCursorL =
  lens fuzzyTimeOfDayCursorTextCursor $ \fdc tc -> fdc {fuzzyTimeOfDayCursorTextCursor = tc}

fuzzyTimeOfDayCursorGuess :: FuzzyTimeOfDayCursor -> Maybe TimeOfDay
fuzzyTimeOfDayCursorGuess FuzzyTimeOfDayCursor {..} = do
  ftod <- parseMaybe fuzzyTimeOfDayP $ rebuildTextCursor fuzzyTimeOfDayCursorTextCursor
  pure $ resolveTimeOfDay fuzzyTimeOfDayCursorBaseTimeOfDay ftod
