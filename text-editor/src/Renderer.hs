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
import UnusedPropInspection
import UndeclaredPropInspection

markupMap :: AttrMap
markupMap = attrMap V.defAttr
  [ ("error", bg V.red)
  , ("warning", bg V.yellow)
  , ("comment", fg $ V.rgbColor 80 80 80)
  , ("ret", fg V.brightMagenta)
  , ("guard", fg V.blue)
  , ("string", fg V.green)
  , ("normal", fg V.brightWhite)
  ]

render :: String -> T.Widget n
render txt =
    let toks = tokenize txt
        tree' = parseLang toks
        
        highlights = case tree' of
            Right tree -> inspect tree
            Left err -> ([], filter (\t -> (errorPos err) == (pos t)) toks)

        tokLines = splitWhen (\t -> tokType t == NL) toks
        widgetLines = (map . map) (\t -> renderToken t highlights) tokLines
        finalLines = map mergeHoriz widgetLines
    in
        mergeVert finalLines
    where
        renderToken tok (warns, errs) =
            if isErr tok errs then
                markup $ (pack $ text tok) @? "error"
            else if elem tok warns then
                markup $ (pack $ text tok) @? "warning"
            else if tokType tok == COMMENT then
                markup $ (pack $ text tok) @? "comment"
            else if tokType tok == CARET then
                markup $ (pack $ text tok) @? "ret"
            else if elem (tokType tok) [RBRACK, LBRACK] then
                markup $ (pack $ text tok) @? "guard"
            else if tokType tok == STRING then
                markup $ (pack $ text tok) @? "string"
            else
                markup $ (pack $ text tok) @? "normal"

        isErr tok errToks = (elem tok errToks) || (elem (tokType tok) [ERR_STRING])

        mergeHoriz [] = str "\n"
        mergeHoriz ws = foldl (<+>) emptyWidget ws
        
        mergeVert ws = foldl (<=>) emptyWidget ws

        inspect tree = joinInspectionResults $ map (\i -> i tree) inspections

        joinInspectionResults = foldl (\(warnsAcc, errsAcc) (warns, errs) -> (warnsAcc ++ warns, errsAcc ++ errs)) ([], [])

        inspections = [inspectUnused, inspectUndeclared]


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
    case break p xs of
        (before, [])     -> [before]  -- If no element satisfies the predicate, return the remainder as a single list
        (before, _:rest) -> before : splitWhen p rest  -- Discard the element that satisfies the predicate and continue splitting
