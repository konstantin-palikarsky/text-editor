{-# LANGUAGE FlexibleContexts #-}
module Lexer
  ( TokenType(..)
  , Token(..)
  , tokenize
  , isAfter
  ) where

import Text.ParserCombinators.Parsec hiding (token, tokens, alphaNum)

data TokenType =  ID     |
                  INT    |
                  SEMICOL|
                  COLON  |
                  LPARENS|
                  RPARENS|
                  LBRACK |
                  RBRACK |
                  EQUAL  |
                  COMMA  |
                  LAMBDA |
                  COMMENT|
                  NL     |
                  WS     |
                  ERR    |
                  EOF
  deriving (Show, Eq)

data Token = Token { tokType :: TokenType
                   , text :: String
                   , pos :: SourcePos
                   } deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize txt = let Right res = parse tokens "" txt in res

isAfter :: Token -> Token -> Bool
isAfter t1 t2 = (pos t1) > (pos t2)

tokens :: Parser [Token]
tokens = do
  toks <- many token
  position <- getPosition
  return $ toks ++ [Token EOF " " position]

token :: Parser Token
token = choice
  [
    accept ID       $ do t <- oneOf alpha ; ts <- many $ oneOf alphaNum ; return $ (t:ts :: String)
  , accept SEMICOL  $ string ";"
  , accept INT      $ many1 $ oneOf ['0'..'9']
  , accept COLON    $ string ":"
  , accept LPARENS  $ string "("
  , accept RPARENS  $ string ")"
  , accept LBRACK   $ string "["
  , accept RBRACK   $ string "]"
  , accept EQUAL    $ string "="
  , accept COMMA    $ string ","
  , accept LAMBDA   $ string "λ"
  , accept COMMENT  $ char '#' *> (many $ noneOf "\n") >>= \comment -> return $ "#" ++ comment
  , accept NL       $ string "\n"
  , accept WS       $ many1 $ oneOf " \t"
  , accept ERR      $ do t <- anyToken ; return [t]
  ]
  where
    alpha = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
    alphaNum = alpha ++ ['0'..'9']

accept :: TokenType -> Parser String -> Parser Token
accept tt p = do
  position <- getPosition
  txt <- p
  return $ Token tt txt position