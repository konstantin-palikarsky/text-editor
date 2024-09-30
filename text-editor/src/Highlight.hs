{-# LANGUAGE OverloadedStrings #-}

module Highlight ( highlightColors, highlightText ) where

import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.Widgets.Core ((<+>), (<=>), str, emptyWidget)
import Brick.Markup (markup, (@?))
import Brick.Util (fg, bg)
import Brick.AttrMap (attrMap, AttrMap)

import Data.Text (Text, pack, unpack)

import Text.Parsec.Error

import Lex
import Parse

-- Attribute map constant, extended with the main colors necessary for text markup
highlightColors :: AttrMap
highlightColors = attrMap V.defAttr
  [ ("error", bg V.red),
    ("comment", fg $ V.rgbColor 80 80 80),
    ("func", fg V.brightMagenta),
    ("sqbracket", fg V.blue),
    ("int", fg V.green),
    ("normal", fg V.brightWhite) ]

-- Highlights the inputed text, by lexing and then parsing it, finally it is recombined to a drawable widget
highlightText :: Text -> T.Widget n
highlightText inputText =
    let tokens = tokenize $ unpack $ inputText
        tree = parseLang tokens
        errors = case tree of
            Right _ -> []
            Left err -> filter (\t -> errorPos err == pos t) tokens -- Find a token at the position of the error

        tokLines = splitWhen (\t -> tType t == NL) tokens
        widgetLines = (map . map) (\t -> highlightTokenByGrammar t errors) tokLines
        finalLines = map mergeHoriz widgetLines
    in
        foldl (<=>) emptyWidget finalLines
    where
        mergeHoriz [] = str "\n"
        mergeHoriz ws = foldl (<+>) emptyWidget ws

-- Internal function to distinguish between token types, and transform them to the expected widget
highlightTokenByGrammar:: Token -> [Token] -> T.Widget a
highlightTokenByGrammar token errors
    | token `elem` errors         = markup $ (pack $ text token) @? "error"
    | tType token == COMMENT      = markup $ (pack $ text token) @? "comment"
    | tType token == INT          = markup $ (pack $ text token) @? "int"
    | tType token == LAMBDA       = markup $ (pack $ text token) @? "func"
    | tType token `elem` [RBRACK, LBRACK] = markup $ (pack $ text token) @? "sqbracket"
    | otherwise                   = markup $ (pack $ text token) @? "normal"

-- Splits a list to a list of lists based on an arbitrary predicate
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
    case break p xs of
        (before, [])     -> [before]
        (before, _:rest) -> before : splitWhen p rest
