{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( Block(..)
    , Commands(..)
    , Command(..)
    , Guard(..)
    , BoolExpr(..)
    , Expression(..)
    , Primary(..)
    , Reference(..)
    , parseLang
    ) where

import Text.Parsec hiding (token, anyToken, satisfy, noneOf, oneOf)
import Lexer

data Block = Block Commands
    deriving (Show, Eq)

data Commands = Cmds [Command]
    deriving (Show, Eq)

data Command = GuardCmd Guard Commands
             | AssgCmd Reference Expression
             | ExprCmd Expression
             | ReturnCmd Expression
    deriving (Show, Eq)

data Guard = Guard [BoolExpr]
    deriving (Show, Eq)

data BoolExpr = BoolEq Expression Expression
    deriving (Show, Eq)

data Expression = AddExpr Primary [Token] [Expression]
    deriving (Show, Eq)

data Primary = StrPrim Token
             | BlockPrim Block
             | RefPrim Reference
             | ParensPrim Expression
    deriving (Show, Eq)

data Reference = Ref [Token] Token
    deriving (Show, Eq)

type Parser a = Parsec [Token] () a

parseLang :: [Token] -> Either ParseError Block
parseLang toks = parse block "" $ filter (not.ignored) toks
    where
        ignored t = elem (tokType t) [WS, NL, COMMENT]

block :: Parser Block
block = between (token LCURLY) (token RCURLY) commands >>= \cmds -> return $ Block cmds

commands :: Parser Commands
commands = many command >>= \cmds -> return $ Cmds cmds

command :: Parser Command
command = guardCmd <|> assgCmd <|> exprCmd <|> retCmd

guardCmd :: Parser Command
guardCmd = between (token LBRACK) (token RBRACK) guardCmd'
    where
        guardCmd' = do
            grd <- guard <* (token COLON)
            cmds <- commands
            return $ GuardCmd grd cmds

guard :: Parser Guard
guard = do
    grd <- boolExpr
    grds <- many (token COMMA *> boolExpr)
    return $ Guard $ grd:grds

boolExpr :: Parser BoolExpr
boolExpr = do
    left <- expr <* (oneOf [EQUAL, HASH])
    right <- expr
    return $ BoolEq left right

assgCmd :: Parser Command
assgCmd = do
    ref <- try (reference <* token EQUAL)
    val <- expr <* token SEMICOL
    return $ AssgCmd ref val

exprCmd :: Parser Command
exprCmd = expr <* token SEMICOL >>= \e -> return $ ExprCmd e

retCmd :: Parser Command
retCmd = token CARET *> expr <* token SEMICOL >>= \e -> return $ ReturnCmd e

expr :: Parser Expression
expr = do
    prim <- primary
    primSuff <- many (token DOT *> token ID)
    exprs <- many (token PLUS *> expr)
    return $ AddExpr prim primSuff exprs

primary :: Parser Primary
primary = (token STRING >>= \t -> return $ StrPrim t)
    <|> (token ERR_STRING >>= \t -> return $ StrPrim t)
    <|> (block >>= \b -> return $ BlockPrim b)
    <|> (reference >>= \r -> return $ RefPrim r)
    <|> (parensExpr >>= \e -> return $ ParensPrim e)
    where
        parensExpr = between (token LPARENS) (token RPARENS) expr

reference :: Parser Reference
reference = do
    stars <- many $ token STAR
    name <- token ID
    return $ Ref stars name


token :: TokenType -> Parser Token
token tt = satisfy (\t -> tokType t == tt)

--notToken :: TokenType -> Parser Token
--notToken tt = satisfy (\t -> tokType t /= tt)

--noneOf :: [TokenType] -> Parser Token
--noneOf tts = satisfy (\t -> not $ elem (tokType t) tts)

oneOf :: [TokenType] -> Parser Token
oneOf tts = satisfy (\t -> elem (tokType t) tts)

--anyToken :: Parser Token
--anyToken = satisfy (\_ -> True)

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim (\t -> text t) 
                      advance
                      (\t -> if f t then Just t else Nothing)
    where
        advance _ _ ((Token _ _ p) : _) = p
        advance p _ [] = p