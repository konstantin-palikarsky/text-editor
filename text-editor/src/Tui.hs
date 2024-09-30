{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Control.Monad (unless, void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Path
import Path.IO
import System.Directory
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

import Highlight

data TuiState = TuiState { stateCursor :: TextFieldCursor,  forceQuit :: Bool } deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

-- Hook for app startup, initializes the app with the initial state in main
tuiApp :: App TuiState e ResourceName
tuiApp = App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const highlightColors
    }

-- The function to be invoked after each state change caused by event handling, renders the text and the cursor
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
 let cursor = stateCursor ts
 in [ highlightText $ rebuildTextFieldCursor $ cursor, selectedTextFieldCursorWidget ResourceName cursor ]

-- Handles each event based on the initial TUI State
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let actionToState :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TuiState)
          actionToState func = do
            let textFieldCursor = stateCursor s
            let textFieldCursor' = fromMaybe textFieldCursor $ func textFieldCursor
            let s' = s {stateCursor = textFieldCursor'}
            continue s'
          wrapToMaybe :: (a -> a) -> (a -> Maybe a)
          wrapToMaybe f x = Just (f x)
       in case vtye of
            EvKey (KChar c) [] -> actionToState $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> actionToState textFieldCursorSelectPrevLine
            EvKey KDown [] -> actionToState textFieldCursorSelectNextLine
            EvKey KRight [] -> actionToState textFieldCursorSelectNextChar
            EvKey KLeft [] -> actionToState textFieldCursorSelectPrevChar
            EvKey KRight [MCtrl] -> actionToState (wrapToMaybe textFieldCursorSelectNextWord)
            EvKey KLeft [MCtrl] -> actionToState (wrapToMaybe textFieldCursorSelectPrevWord)
            EvKey KBS [] -> actionToState $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> actionToState $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> actionToState $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> halt s
            EvKey KDel [MShift] -> halt s {forceQuit = True }
            _ -> continue s
    _ -> continue s

-- Main function, reads in the expected file, and either stores or clearly exits when done
tui :: IO ()
tui = do
  args <- getArgs
  case args of
    [] -> die "Please specify the path of the file you wish to edit."
    (fp : _) -> do
      path <- resolveFile' fp
      maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
      let contents = fromMaybe "" maybeContents
      initialState <- buildInitialState contents
      endState <- defaultMain tuiApp initialState
      let contents' = rebuildTextFieldCursor (stateCursor endState)
      unless (contents == contents' || (forceQuit endState)) $ T.writeFile (fromAbsFile path) contents'

-- Creates the initial state from default values, and a cursor created from the contents of the file
buildInitialState :: Text -> IO TuiState
buildInitialState contents =
  return TuiState {stateCursor = makeTextFieldCursor contents, forceQuit = False}
