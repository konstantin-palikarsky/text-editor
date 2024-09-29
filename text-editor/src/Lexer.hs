{-# LANGUAGE FlexibleContexts #-}
module Lexer
  ( TokenType(..)
  , Token(..)
  , tokenize
  , isAfter
  ) where

import Text.ParserCombinators.Parsec hiding (token, tokens, alphaNum)

data TokenType = ID | STAR | LCURLY | RCURLY | SEMICOL | CARET 
               | LBRACK| RBRACK | COLON | EQUAL | HASH | COMMA | DOT 
               | LPARENS | RPARENS | PLUS | STRING | ERR_STRING
               | NL | WS | COMMENT | ERR | EOF
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
  [ accept STAR $ string "*"
  , accept LCURLY $ string "{"
  , accept RCURLY $ string "}"
  , accept SEMICOL $ string ";"
  , accept CARET $ string "^"
  , accept LBRACK $ string "["
  , accept RBRACK $ string "]"
  , accept COLON $ string ":"
  , accept EQUAL $ string "="
  , accept HASH $ string "#"
  , accept COMMA $ string ","
  , accept DOT $ string "."
  , accept LPARENS $ string "("
  , accept RPARENS $ string ")"
  , accept PLUS $ string "+"
  , accept COMMENT $ char '%' *> (many $ noneOf "\n") >>= \comment -> return $ "%" ++ comment
  , accept STRING $ try $ do str <- strPrefix <* char '"' ; return $ "\"" ++ str ++ "\""
  , accept ERR_STRING $ try $ do str <- strPrefix <* (notFollowedBy $ char '"') ; return $ "\"" ++ str
  , accept NL $ string "\n"
  , accept WS $ many1 $ oneOf " \t"
  , accept ID $ do t <- oneOf alpha ; ts <- many $ oneOf alphaNum ; return $ (t:ts :: String)
  , accept ERR $ do t <- anyToken ; return [t]
  ]
  where
    alpha = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
    alphaNum = alpha ++ ['0'..'9']
    strPrefix = char '"' *> many (noneOf "\"\n")

accept :: TokenType -> Parser String -> Parser Token
accept tt p = do
  position <- getPosition
  txt <- p
  return $ Token tt txt position