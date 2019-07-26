{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyLocalTime
  ( FuzzyLocalTimeCursor(..)
  , emptyFuzzyLocalTimeCursor
  , makeFuzzyLocalTimeCursor
  , rebuildFuzzyLocalTimeCursor
  , fuzzyLocalTimeCursorTextCursorL
  , fuzzyLocalTimeCursorGuess
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

data FuzzyLocalTimeCursor =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor :: TextCursor
    , fuzzyLocalTimeCursorBaseLocalTime :: LocalTime
    }
  deriving (Show, Eq, Generic)

instance Validity FuzzyLocalTimeCursor

emptyFuzzyLocalTimeCursor :: LocalTime -> FuzzyLocalTimeCursor
emptyFuzzyLocalTimeCursor d =
  FuzzyLocalTimeCursor
    {fuzzyLocalTimeCursorTextCursor = emptyTextCursor, fuzzyLocalTimeCursorBaseLocalTime = d}

makeFuzzyLocalTimeCursor :: LocalTime -> FuzzyLocalTimeCursor
makeFuzzyLocalTimeCursor d =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor =
        fromJust $ makeTextCursor $ T.pack $ formatTime defaultTimeLocale "%F %T%Q" d
    , fuzzyLocalTimeCursorBaseLocalTime = d
    }

rebuildFuzzyLocalTimeCursor :: FuzzyLocalTimeCursor -> AmbiguousLocalTime
rebuildFuzzyLocalTimeCursor fdc@FuzzyLocalTimeCursor {..} =
  fromMaybe (BothTimeAndDay fuzzyLocalTimeCursorBaseLocalTime) $ fuzzyLocalTimeCursorGuess fdc

fuzzyLocalTimeCursorTextCursorL :: Lens' FuzzyLocalTimeCursor TextCursor
fuzzyLocalTimeCursorTextCursorL =
  lens fuzzyLocalTimeCursorTextCursor $ \fdc tc -> fdc {fuzzyLocalTimeCursorTextCursor = tc}

fuzzyLocalTimeCursorGuess :: FuzzyLocalTimeCursor -> Maybe AmbiguousLocalTime
fuzzyLocalTimeCursorGuess FuzzyLocalTimeCursor {..} = do
  ftod <- parseMaybe fuzzyLocalTimeP $ rebuildTextCursor fuzzyLocalTimeCursorTextCursor
  pure $ resolveLocalTime fuzzyLocalTimeCursorBaseLocalTime ftod
