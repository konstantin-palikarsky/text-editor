{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Skylighting (highlight, attrMappingsForStyle)
import Control.Monad (unless, void)
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V
import Path
import Path.IO
import qualified Skylighting.Core as S
import Skylighting.Types (Syntax, Style)
import System.Directory
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

data TuiState = TuiState { stateCursor :: TextFieldCursor, syntax :: Syntax } deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

styles :: [(Text, Skylighting.Types.Style)]
styles = [ ("pygments", S.pygments) ]

buildInitialState :: Text -> Syntax -> IO TuiState
buildInitialState contents blubSyntax = do
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc, syntax = blubSyntax}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let codeText = rebuildTextFieldCursor (stateCursor ts)
      currentStyle = snd (head styles)
   in [highlight (syntax ts) codeText]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo ::
            (TextFieldCursor -> Maybe TextFieldCursor) ->
            EventM n (Next TuiState)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let s' = s {stateCursor = tfc'}
            continue s'
       in case vtye of
            EvKey (KChar c) [] -> mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> mDo textFieldCursorSelectNextLine
            EvKey KRight [] -> mDo textFieldCursorSelectNextChar
            EvKey KLeft [] -> mDo textFieldCursorSelectPrevChar
            EvKey KBS [] -> mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> mDo $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> mDo $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> halt s
            _ -> continue s
    _ -> continue s


tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = \s -> attrMap V.defAttr $ attrMappingsForStyle (snd (head styles))
    }

tui :: IO ()
tui = do
  args <- getArgs
  case args of
    [] -> die "No argument to choose file to edit."
    (fp : _) -> do
      path <- resolveFile' fp
      maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
      let contents = fromMaybe "" maybeContents
      let syntaxDir = "/d/Uni/Masters/10_Semester/PLs/text-editor/definitions" -- Absolute path due to stack behaving weirdly with WSL
      result <- S.loadSyntaxesFromDir syntaxDir
      case result of
        Left e -> putStrLn ("Failed to load syntax map: " ++ e) >> exitFailure
        Right syntaxMap -> do
          let blubSyntax = fromMaybe (error "Syntax not found") $ S.syntaxByName syntaxMap "Blub"
          initialState <- buildInitialState contents blubSyntax
          void $ defaultMain tuiApp initialState
