{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( Program(..)
    , Statements(..)
    , Statement(..)
    , Expression(..)
    , Element(..)
    , Parameter(..)
    , Reference(..)
    , Record(..)
    , Function(..)
    , parseLang
    ) where

import Text.Parsec hiding (token, anyToken, satisfy, noneOf, oneOf)
import Lexer

-- AST Definitions
data Program = Program Statements
    deriving (Show, Eq)

data Statements = Stmts [Statement]
    deriving (Show, Eq)

data Statement = AssgStmt Reference Expression
               | ExprStmt Expression
    deriving (Show, Eq)

data Expression = Expr [Element]
    deriving (Show, Eq)

data Element = IntElem Token
             | RefElem Reference
             | ParensElem Expression
             | FuncElem Function
             | RecElem Record
    deriving (Show, Eq)

data Record = Record [(Reference, Expression)]
    deriving (Show, Eq)

data Function = Lambda [Parameter] Expression
    deriving (Show, Eq)

data Parameter = Param Reference
    deriving (Show, Eq)

data Reference = Ref Token
    deriving (Show, Eq)

-- Parser Type
type Parser a = Parsec [Token] () a

-- Main parser function
parseLang :: [Token] -> Either ParseError Program
parseLang toks = parse program "" $ filter (not . ignored) toks
    where
        ignored t = elem (tokType t) [WS, NL, COMMENT]

-- Program parser (list of statements)
program :: Parser Program
program = many statement >>= \stmts -> return $ Program (Stmts stmts)

-- Statement parser (assignment or expression)
statement :: Parser Statement
statement = try assgStmt <|> exprStmt

-- Assignment statement parser
assgStmt :: Parser Statement
assgStmt = do
    ref <- try (reference <* token EQUAL)
    val <- expression <* token SEMICOL
    return $ AssgStmt ref val

-- Expression statement parser
exprStmt :: Parser Statement
exprStmt = expression <* token SEMICOL >>= \e -> return $ ExprStmt e

-- Expression parser
expression :: Parser Expression
expression = many1 element >>= \elems -> return $ Expr elems

-- Element parser
element :: Parser Element
element = (token INT >>= return . IntElem)
      <|> (reference >>= return . RefElem)
      <|> (parensExpr >>= return . ParensElem)
      <|> (function >>= return . FuncElem)
      <|> (record >>= return . RecElem)
    where
        parensExpr = between (token LPARENS) (token RPARENS) expression

-- Record parser
record :: Parser Record
record = between (token LBRACK) (token RBRACK) (sepBy recordEntry (token COMMA)) >>= return . Record
    where
        recordEntry = do
            ref <- reference
            token EQUAL
            expr <- expression
            return (ref, expr)

-- Function parser
function :: Parser Function
function = do
    token LAMBDA
    params <- between (token LPARENS) (token RPARENS) (sepBy parameter (token COMMA))
    token COLON
    body <- expression
    return $ Lambda params body

-- Parameter parser
parameter :: Parser Parameter
parameter = reference >>= return . Param

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
