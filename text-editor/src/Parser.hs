{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( Block(..)
    , Commands(..)
    , Command(..)
    , Expression(..)
    , Primary(..)
    , Reference(..)
    , parseLang
    ) where

import Text.Parsec hiding (token, anyToken, satisfy, noneOf, oneOf)
import Lexer

-- AST Definitions
data Block = Block Commands
    deriving (Show, Eq)

data Commands = Cmds [Command]
    deriving (Show, Eq)

data Command = AssgCmd Reference Expression
             | ExprCmd Expression
    deriving (Show, Eq)

data Expression = PrimExpr Primary
    deriving (Show, Eq)

data Primary = IntPrim Token
             | RefPrim Reference
             | ParensPrim Expression
    deriving (Show, Eq)

data Reference = Ref Token
    deriving (Show, Eq)

-- Parser Type
type Parser a = Parsec [Token] () a

-- Main parser function
parseLang :: [Token] -> Either ParseError Block
parseLang toks = parse commands "" $ filter (not . ignored) toks
    where
        ignored t = elem (tokType t) [WS, NL, COMMENT]

-- Commands parser
commands :: Parser Block
commands = many command >>= \cmds -> return $ Block (Cmds cmds)

-- Command parser, only assignment and expression commands
command :: Parser Command
command = assgCmd <|> exprCmd

-- Assignment command parser
assgCmd :: Parser Command
assgCmd = do
    ref <- try (reference <* token EQUAL)
    val <- expr <* token SEMICOL
    return $ AssgCmd ref val

-- Expression command parser
exprCmd :: Parser Command
exprCmd = expr <* token SEMICOL >>= \e -> return $ ExprCmd e

-- Expression parser
expr :: Parser Expression
expr = PrimExpr <$> primary

-- Primary parser (Integers, References, or Parenthesized Expressions)
primary :: Parser Primary
primary = (token INT >>= return . IntPrim)
      <|> (reference >>= return . RefPrim)
      <|> (parensExpr >>= return . ParensPrim)
    where
        parensExpr = between (token LPARENS) (token RPARENS) expr

-- Reference parser (e.g., names, variables)
reference :: Parser Reference
reference = token ID >>= return . Ref

-- Token parsers
token :: TokenType -> Parser Token
token tt = satisfy (\t -> tokType t == tt)

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim (\t -> text t)
                      advance
                      (\t -> if f t then Just t else Nothing)
    where
        advance _ _ ((Token _ _ p) : _) = p
        advance p _ [] = p
