{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.Widgets.Core ((<+>), (<=>), str, emptyWidget)
import Brick.Markup (markup, (@?))
import Brick.Util (fg, bg)
import Brick.AttrMap (attrMap, AttrMap)

import Data.Text (pack)

import Text.Parsec.Error

import Lexer
import Parser

markupMap :: AttrMap
markupMap = attrMap V.defAttr
  [ ("error", bg V.red)
  , ("comment", fg $ V.rgbColor 80 80 80)
  , ("ret", fg V.brightMagenta)
  , ("guard", fg V.blue)
  , ("int", fg V.green)
  , ("normal", fg V.brightWhite)
  ]

render :: String -> T.Widget n
render txt =
    let toks = tokenize txt
        tree' = parseLang toks

        highlights = case tree' of
            Right _ -> ([], [])  -- No warnings or errors from the parser itself
            Left err -> ([], filter (\t -> errorPos err == pos t) toks)

        tokLines = splitWhen (\t -> tokType t == NL) toks
        widgetLines = (map . map) (\t -> renderToken t highlights) tokLines
        finalLines = map mergeHoriz widgetLines
    in
        mergeVert finalLines
    where
        renderToken tok (_, errs) =
            if isErr tok errs then
                markup $ (pack $ text tok) @? "error"
            else if tokType tok == COMMENT then
                markup $ (pack $ text tok) @? "comment"
            else if tokType tok == INT then
                markup $ (pack $ text tok) @? "int"
            else if tokType tok == LAMBDA then
                markup $ (pack $ text tok) @? "ret"
            else if elem (tokType tok) [RBRACK, LBRACK] then
                markup $ (pack $ text tok) @? "guard"
            else
                markup $ (pack $ text tok) @? "normal"

        isErr tok errToks = elem tok errToks

        mergeHoriz [] = str "\n"
        mergeHoriz ws = foldl (<+>) emptyWidget ws

        mergeVert ws = foldl (<=>) emptyWidget ws

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
    case break p xs of
        (before, [])     -> [before]
        (before, _:rest) -> before : splitWhen p rest
