{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyLocalTime
  ( FuzzyLocalTimeCursor (..),
    emptyFuzzyLocalTimeCursor,
    makeFuzzyLocalTimeCursor,
    rebuildFuzzyLocalTimeCursor,
    fuzzyLocalTimeCursorTextCursorL,
    fuzzyLocalTimeCursorGuess,
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

data FuzzyLocalTimeCursor = FuzzyLocalTimeCursor
  { fuzzyLocalTimeCursorTextCursor :: TextCursor,
    fuzzyLocalTimeCursorBaseLocalTime :: LocalTime
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyLocalTimeCursor

instance NFData FuzzyLocalTimeCursor

emptyFuzzyLocalTimeCursor :: LocalTime -> FuzzyLocalTimeCursor
emptyFuzzyLocalTimeCursor d =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor = emptyTextCursor,
      fuzzyLocalTimeCursorBaseLocalTime = d
    }

makeFuzzyLocalTimeCursor :: AmbiguousLocalTime -> FuzzyLocalTimeCursor
makeFuzzyLocalTimeCursor alt =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor =
        fromJust $
          makeTextCursor $
            T.pack $
              case alt of
                OnlyDaySpecified d -> formatTime defaultTimeLocale "%F" d
                BothTimeAndDay lt -> formatTime defaultTimeLocale "%F %T%Q" lt,
      fuzzyLocalTimeCursorBaseLocalTime =
        case alt of
          OnlyDaySpecified d -> LocalTime d midnight
          BothTimeAndDay lt -> lt
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
