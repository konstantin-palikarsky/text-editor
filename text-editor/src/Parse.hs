module Parse ( parseLang ) where

import Text.Parsec hiding (token, anyToken, satisfy, noneOf, oneOf)
import Lex

{-
   The grammar of the language, described as Haskell data classes and their constructors
-}

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

type Parser a = Parsec [Token] () a

-- Main parsing function, returns the full AST, or a specific error if the AST could not be reliably built
parseLang :: [Token] -> Either ParseError Program
parseLang toks = parse program "" $ filter (not . ignored) toks
    where
        ignored t = elem (tType t) [WS, NL, COMMENT]


{-
   Parsec boilerplate, which designates a combinator to use (many, try) and breaks the problem into smaller parsers,
   to recursively descend to the appropriate primitive, while building the AST along the way
-}

program :: Parser Program
program = many statement >>= \stmts -> return $ Program (Stmts stmts)


statement :: Parser Statement
statement = try assgStmt <|> exprStmt


assgStmt :: Parser Statement
assgStmt = do
    ref <- try (reference <* token EQUAL)
    val <- expression <* token SEMICOL
    return $ AssgStmt ref val


exprStmt :: Parser Statement
exprStmt = expression <* token SEMICOL >>= \e -> return $ ExprStmt e


expression :: Parser Expression
expression = many1 element >>= \elems -> return $ Expr elems


element :: Parser Element
element = (token INT >>= return . IntElem)
      <|> (reference >>= return . RefElem)
      <|> (parensExpr >>= return . ParensElem)
      <|> (function >>= return . FuncElem)
      <|> (record >>= return . RecElem)
    where
        parensExpr = between (token LPARENS) (token RPARENS) expression


record :: Parser Record
record = between (token LBRACK) (token RBRACK) (sepBy recordEntry (token COMMA)) >>= return . Record
    where
        recordEntry = do
            ref <- reference
            token EQUAL
            expr <- expression
            return (ref, expr)


function :: Parser Function
function = do
    token LAMBDA
    params <- between (token LPARENS) (token RPARENS) (sepBy parameter (token COMMA))
    token COLON
    body <- expression
    return $ Lambda params body


parameter :: Parser Parameter
parameter = reference >>= return . Param


reference :: Parser Reference
reference = token ID >>= return . Ref

-- A wrapper for satisfy, so that the token can be requested simply by type
token :: TokenType -> Parser Token
token tt = satisfy (\t -> tType t == tt)

-- Attempts to read in a token that satisfies the expected type
satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim (\t -> text t)
                      advance
                      (\t -> if f t then Just t else Nothing)
    where
        advance _ _ ((Token _ _ p) : _) = p
        advance p _ [] = p
