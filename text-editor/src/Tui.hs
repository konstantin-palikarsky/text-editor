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
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Path
import Path.IO
import System.Directory
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

import Renderer as R

data TuiState = TuiState { stateCursor :: TextFieldCursor,  forceQuit :: Bool } deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp = App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const R.markupMap
    }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [
  (R.render (unpack (rebuildTextFieldCursor (stateCursor ts)))),
  selectedTextFieldCursorWidget ResourceName (stateCursor ts)
  ]

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

buildInitialState :: Text -> IO TuiState
buildInitialState contents =
  return TuiState {stateCursor = makeTextFieldCursor contents, forceQuit = False}
