{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyTime.Demo
  ( demo
  ) where

import Data.Maybe

import Data.Function
import Data.Time

import Lens.Micro

import Control.Monad

import Cursor.FuzzyDay
import Cursor.FuzzyLocalTime
import Cursor.FuzzyTimeOfDay
import Cursor.Text
import Cursor.Types

import Brick as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

demo :: IO ()
demo = do
  lt <- (\zt -> utcToLocalTime (zonedTimeZone zt) (zonedTimeToUTC zt)) <$> getZonedTime
  void $
    Brick.defaultMain picoSmosApp $
    State
      { stateDayCursor = makeFuzzyDayCursor $ localDay lt
      , stateTimeOfDayCursor = makeFuzzyTimeOfDayCursor $ localTimeOfDay lt
      , stateLocalTimeCursor = makeFuzzyLocalTimeCursor lt
      , stateSelection = SelectLocalTime
      }

picoSmosApp :: App State e Selection
picoSmosApp =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

data State =
  State
    { stateDayCursor :: FuzzyDayCursor
    , stateTimeOfDayCursor :: FuzzyTimeOfDayCursor
    , stateLocalTimeCursor :: FuzzyLocalTimeCursor
    , stateSelection :: Selection
    }
  deriving (Show, Eq)

stateCurrentTC :: Lens' State TextCursor
stateCurrentTC = lens getter setter
  where
    getter :: State -> TextCursor
    getter s =
      s &
      case stateSelection s of
        SelectDay -> fuzzyDayCursorTextCursor . stateDayCursor
        SelectTimeOfDay -> fuzzyTimeOfDayCursorTextCursor . stateTimeOfDayCursor
        SelectLocalTime -> fuzzyLocalTimeCursorTextCursor . stateLocalTimeCursor
    setter :: State -> TextCursor -> State
    setter s tc =
      case stateSelection s of
        SelectDay -> s {stateDayCursor = stateDayCursor s & fuzzyDayCursorTextCursorL .~ tc}
        SelectTimeOfDay ->
          s {stateTimeOfDayCursor = stateTimeOfDayCursor s & fuzzyTimeOfDayCursorTextCursorL .~ tc}
        SelectLocalTime ->
          s {stateLocalTimeCursor = stateLocalTimeCursor s & fuzzyLocalTimeCursorTextCursorL .~ tc}

data Selection
  = SelectDay
  | SelectTimeOfDay
  | SelectLocalTime
  deriving (Show, Eq, Ord)

selectNext :: Selection -> Selection
selectNext s =
  case s of
    SelectDay -> SelectTimeOfDay
    SelectTimeOfDay -> SelectLocalTime
    SelectLocalTime -> SelectDay

draw :: State -> [Widget Selection]
draw State {..} =
  [ centerLayer $
    border $
    padAll 1 $
    vBox
      [ guessCursorWidget
          (selectIf SelectDay)
          fuzzyDayCursorTextCursor
          fuzzyDayCursorGuess
          stateDayCursor
          SelectDay
      , str " "
      , guessCursorWidget
          (selectIf SelectTimeOfDay)
          fuzzyTimeOfDayCursorTextCursor
          fuzzyTimeOfDayCursorGuess
          stateTimeOfDayCursor
          SelectTimeOfDay
      , str " "
      , guessCursorWidget
          (selectIf SelectLocalTime)
          fuzzyLocalTimeCursorTextCursor
          fuzzyLocalTimeCursorGuess
          stateLocalTimeCursor
          SelectLocalTime
      ]
  ]
  where
    selectIf s = stateSelection == s

guessCursorWidget :: Show t => Bool -> (a -> TextCursor) -> (a -> Maybe t) -> a -> n -> Widget n
guessCursorWidget selected tcf gf c n =
  let tc = tcf c
   in vBox
        [ hCenter $
          (if selected
             then (showCursor n (Location (textCursorIndex tc, 0)))
             else id) $
          txtWrap $
          case rebuildTextCursor tc of
            "" -> " "
            t -> t
        , hCenter $
          case gf c of
            Nothing -> txt " "
            Just d -> str $ show d
        ]

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent s@State {..} e = do
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key _ ->
          let pDo :: (TextCursor -> TextCursor) -> EventM n (Next State)
              pDo func = continue (s & stateCurrentTC %~ func)
              mDo :: (TextCursor -> Maybe TextCursor) -> EventM n (Next State)
              mDo func = continue $ fromMaybe s (s & stateCurrentTC func)
              mDDo :: (TextCursor -> Maybe (DeleteOrUpdate TextCursor)) -> EventM n (Next State)
              mDDo f = mDo (dullMDelete . f)
           in case key of
                KChar '\t' -> continue $ s {stateSelection = selectNext stateSelection}
                KChar c -> mDo $ textCursorInsert c
                KLeft -> mDo textCursorSelectPrev
                KRight -> mDo textCursorSelectNext
                KHome -> pDo textCursorSelectStart
                KEnd -> pDo textCursorSelectEnd
                KBS -> mDDo textCursorRemove
                KDel -> mDDo textCursorDelete
                KEsc -> halt s
                KEnter -> halt s
                _ -> continue s
        _ -> continue s
    _ -> continue s
