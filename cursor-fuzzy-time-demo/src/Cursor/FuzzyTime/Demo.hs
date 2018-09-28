{-# LANGUAGE OverloadedStrings #-}

module Cursor.FuzzyTime.Demo
    ( demo
    ) where

import Data.Maybe

import Data.Function
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import System.Directory
import System.Environment
import System.Exit

import Lens.Micro

import Control.Monad

import Cursor.FuzzyDay
import Cursor.Text

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

demo :: IO ()
demo = do
    today <- getCurrentDay
    void $ Brick.defaultMain picoSmosApp (today, emptyFuzzyDayCursor)

picoSmosApp :: App (Day, FuzzyDayCursor) e Text
picoSmosApp =
    App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap Vty.defAttr []
        }

draw :: (Day, FuzzyDayCursor) -> [Widget Text]
draw (today, fdc) =
    let tc = fuzzyDayCursorTextCursor fdc
     in [ centerLayer $
          border $
          padAll 1 $
          vBox
              [ hCenter $
                showCursor "cursor" (Location (textCursorIndex tc, 0)) $
                txtWrap $
                case rebuildTextCursor tc of
                    "" -> " "
                    t -> t
              , hCenter $ case fuzzyDayCursorGuess today fdc of
                    Nothing -> txt " "
                    Just d -> str $ show d
              ]
        ]

handleEvent ::
       (Day, FuzzyDayCursor)
    -> BrickEvent Text e
    -> EventM Text (Next (Day, FuzzyDayCursor))
handleEvent (d, fdc) e = do
    fdc' <-
        case e of
            VtyEvent ve ->
                case ve of
                    EvKey key mods ->
                        let pDo :: (TextCursor -> TextCursor)
                                -> EventM Text (Next FuzzyDayCursor)
                            pDo func =
                                continue
                                    (fdc & fuzzyDayCursorTextCursorL %~ func)
                            mDo :: (TextCursor -> Maybe TextCursor)
                                -> EventM Text (Next FuzzyDayCursor)
                            mDo func =
                                continue $
                                fromMaybe
                                    fdc
                                    (fdc & fuzzyDayCursorTextCursorL func)
                         in case key of
                                KChar c -> mDo $ textCursorInsert c
                                KLeft -> mDo textCursorSelectPrev
                                KRight -> mDo textCursorSelectNext
                                KBS -> mDo textCursorRemove
                                KHome -> pDo textCursorSelectStart
                                KEnd -> pDo textCursorSelectEnd
                                KDel -> mDo textCursorDelete
                                KEsc -> halt fdc
                                KEnter -> halt fdc
                                _ -> continue fdc
                    _ -> continue fdc
            _ -> continue fdc
    pure $ (,) d <$> fdc'

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime
