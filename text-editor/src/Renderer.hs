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
  , ("func", fg V.brightMagenta)
  , ("sqbracket", fg V.blue)
  , ("int", fg V.green)
  , ("normal", fg V.brightWhite)
  ]

render :: String -> T.Widget n
render txt =
    let tokens = tokenize txt
        tree = parseLang tokens
        highlights = case tree of
            Right _ -> ([], [])
            Left err -> ([], filter (\t -> errorPos err == pos t) tokens)

        tokLines = splitWhen (\t -> tokType t == NL) tokens
        widgetLines = (map . map) (\t -> renderToken t highlights) tokLines
        finalLines = map mergeHoriz widgetLines
    in
        mergeVert finalLines
    where
        renderToken tok (_, errs)
            | isErr tok errs              = markup $ (pack $ text tok) @? "error"
            | tokType tok == COMMENT      = markup $ (pack $ text tok) @? "comment"
            | tokType tok == INT          = markup $ (pack $ text tok) @? "int"
            | tokType tok == LAMBDA       = markup $ (pack $ text tok) @? "func"
            | tokType tok `elem` [RBRACK, LBRACK] = markup $ (pack $ text tok) @? "sqbracket"
            | otherwise                   = markup $ (pack $ text tok) @? "normal"

        isErr tok errtokens = elem tok errtokens

        mergeHoriz [] = str "\n"
        mergeHoriz ws = foldl (<+>) emptyWidget ws

        mergeVert ws = foldl (<=>) emptyWidget ws


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
    case break p xs of
        (before, [])     -> [before]
        (before, _:rest) -> before : splitWhen p rest
