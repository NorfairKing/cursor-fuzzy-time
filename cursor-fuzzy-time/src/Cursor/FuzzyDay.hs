{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyDay
  ( FuzzyDayCursor (..),
    emptyFuzzyDayCursor,
    makeFuzzyDayCursor,
    rebuildFuzzyDayCursor,
    fuzzyDayCursorTextCursorL,
    fuzzyDayCursorGuess,
  )
where

import Control.DeepSeq
import Cursor.Text
import Data.FuzzyTime
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Text.Megaparsec

data FuzzyDayCursor = FuzzyDayCursor
  { fuzzyDayCursorTextCursor :: TextCursor,
    fuzzyDayCursorBaseDay :: Day
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyDayCursor

instance NFData FuzzyDayCursor

emptyFuzzyDayCursor :: Day -> FuzzyDayCursor
emptyFuzzyDayCursor d =
  FuzzyDayCursor
    { fuzzyDayCursorTextCursor = emptyTextCursor,
      fuzzyDayCursorBaseDay = d
    }

makeFuzzyDayCursor :: Day -> FuzzyDayCursor
makeFuzzyDayCursor d =
  FuzzyDayCursor
    { fuzzyDayCursorTextCursor =
        fromJust $
          makeTextCursor $
            T.pack $
              formatTime defaultTimeLocale "%F" d,
      fuzzyDayCursorBaseDay = d
    }

rebuildFuzzyDayCursor :: FuzzyDayCursor -> Day
rebuildFuzzyDayCursor fdc@FuzzyDayCursor {..} =
  fromMaybe fuzzyDayCursorBaseDay $ fuzzyDayCursorGuess fdc

fuzzyDayCursorTextCursorL :: Lens' FuzzyDayCursor TextCursor
fuzzyDayCursorTextCursorL =
  lens fuzzyDayCursorTextCursor $ \fdc tc ->
    fdc {fuzzyDayCursorTextCursor = tc}

fuzzyDayCursorGuess :: FuzzyDayCursor -> Maybe Day
fuzzyDayCursorGuess FuzzyDayCursor {..} = do
  fd <- parseMaybe fuzzyDayP $ rebuildTextCursor fuzzyDayCursorTextCursor
  pure $ resolveDay fuzzyDayCursorBaseDay fd
