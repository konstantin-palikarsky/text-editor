module Lex ( TokenType(..), Token(..), tokenize ) where

import Text.ParserCombinators.Parsec hiding (token, tokens)

-- All syntactic tokens that our language recognizes
data TokenType =  ID      |
                  INT     |
                  SEMICOL |
                  COLON   |
                  LPARENS |
                  RPARENS |
                  LBRACK  |
                  RBRACK  |
                  EQUAL   |
                  COMMA   |
                  LAMBDA  |
                  COMMENT |
                  NL      |
                  WS      |
                  ERR
  deriving (Show, Eq)

-- We define a token as an aggregate of a type, its textual contents and its position
data Token = Token { tType :: TokenType,
                     text :: String,
                     pos :: SourcePos }
  deriving (Show, Eq)

-- Main lexing function, invokes the parsec tokenizer
tokenize :: String -> [Token]
tokenize inputText = let Right res = parse tokens "" inputText in res

-- Generates every token for the inputted text
tokens :: Parser [Token]
tokens = do
  allTokens <- many token
  return $ allTokens

-- The tokenizing matching logic, describes all expected inputs and connects them to their appropriate type
token :: Parser Token
token = choice
    [
    accept ID       $ do t <- oneOf alpha ; ts <- many $ oneOf alphaNum ; return $ (t:ts :: String),
    accept SEMICOL  $ string ";",
    accept INT      $ many1 $ oneOf num,
    accept COLON    $ string ":",
    accept LPARENS  $ string "(",
    accept RPARENS  $ string ")",
    accept LBRACK   $ string "[",
    accept RBRACK   $ string "]",
    accept EQUAL    $ string "=",
    accept COMMA    $ string ",",
    accept LAMBDA   $ string "λ",
    accept COMMENT  $ char '#' *> (many $ noneOf "\n") >>= \comment -> return $ "#" ++ comment,
    accept NL       $ string "\n",
    accept WS       $ many1 $ oneOf " \t",
    accept ERR      $ do t <- anyToken ; return [t] -- Error token if nothing matches the expected result, should never be created
    ]
  where
    alpha = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
    num   = ['0'..'9']
    alphaNum = alpha ++ num

-- Generate a parser token from a token type and a string ingested by the parser
accept :: TokenType -> Parser String -> Parser Token
accept tt p = do
  position <- getPosition
  txt <- p
  return $ Token tt txt position