{-# LANGUAGE OverloadedStrings #-}
module Parser where

import SyntaxTree
import Type
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
import Data.Void
import Data.Functor

type Parser = Parsec Void T.Text


-- First of all, let us specify the syntax we want to parse
--

{-
boolean := `true` | `false`
integer := [0-9]+
float   := [0-9]+ `.` [0-9]+
string  := `"` [^"]* `"`

reserved_keywords := `cond`, `lam`, `let`, `true`, `false`. `else`, `def`, `fix`, `equal`
ident   := ID_Start ID_Continue* but not reserved_keywords

type  := facile

cond   := `cond` `{` (expr `->` `{` expr `}` `}`
let    := `let` (decl)* `{` expr `}`
lambda := `lam` `[` (ident `:` type)+ `]` `{` expr `}`
fix    := `fix` expr
equal  := `equal` expr expr

expr   := cond | let | lambda | ident | fix | equal
        | `(` expr `)`
        | expr expr

decl   := `def` ident `=` expr
        | `def` ident '[' (ident `:` type)+ `]` `{` expr `}`
-}

-- Now let us deal with the lexical structure of our language,
-- it can be tricky, but cool.

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer


boolean :: Parser ELit
boolean = (symbol "true" $> EBool True) <|> (symbol "false" $> EBool False)

-- Watchout, float must be executed before integer, to parse the longest matching prefix
integer :: Parser ELit
integer = EInt <$> lexeme L.decimal

float :: Parser ELit
float = EFloat <$> lexeme L.float

stringLit :: Parser ELit
stringLit = fmap (EString . T.pack) (lexeme (char '"' *> manyTill L.charLiteral (char '"')))

literal :: Parser Expr
literal = ELiteral <$> choice
    [ boolean
    , try float -- If float fails, it could still be an integer. TODO: THere is a more efficient way
    , integer
    , stringLit
    ]

reservedKeywords :: [T.Text]
reservedKeywords =
    [ "cond"
    , "lam"
    , "let"
    , "true"
    , "false"
    , "else"
    , "def"
    , "fix"
    ]

identifier :: Parser T.Text
identifier = lexeme $ do
    let init = letterChar <|> char '_'
    firstChar <- init
    rest <- many alphaNumChar
    let name = T.pack (firstChar : rest)
    if name `elem` reservedKeywords
    then fail "Reserved keyword can not be used as identifier"
    else return name


-- The parser grammar is an operator grammar
typeParser :: Parser Type
typeParser = do
    let parseArrow = symbol "->" $> TFunction
        parseTerm = choice
            [ symbol "Bool" $> TBool
            , symbol "String" $> TString
            , symbol "Int" $> TInt
            , symbol "Float" $> TFloat
            ]
    let ops = [[E.InfixR parseArrow]]
    E.makeExprParser parseTerm ops

block :: Parser a -> Parser a
block = between (symbol "{") (symbol "}")

cond :: Parser Expr
cond = do
        symbol "cond"
        block $ do
            clauses <- manyTill clause (symbol "else")
            symbol "~>"
            elseClause <- block expr
            return $ ECond clauses elseClause
    where
        clause = do
            cond <- expr
            symbol "~>"
            body <- block expr
            return (cond, body)

letParser :: Parser Expr
letParser = do
    symbol "let"
    decls <- manyTill decl (symbol "{")
    body <- expr
    symbol "}"
    return $ ELet decls body

args :: Parser [(T.Text, Type)]
args = between (symbol "[") (symbol "]") $ sepBy1 parseArgs (symbol ",")
    where parseArgs = do
            name <- identifier
            symbol ":"
            ty <- typeParser
            return (name, ty)

lambda :: Parser Expr
lambda = do
    symbol "lam"
    params <- args
    body <- block expr
    return $ ELambda params body

fix :: Parser Expr
fix = do
    symbol "fix"
    EFix <$> expr

builtin :: Parser Expr
builtin = choice
        [ symbol "+" $> EBuiltinOp ESum
        , symbol "-" $> EBuiltinOp ESub
        , symbol "*" $> EBuiltinOp EProd
        , symbol "/" $> EBuiltinOp EDiv
        , symbol "&&" $> EBuiltinOp EAnd
        , symbol "||" $> EBuiltinOp EOr
        , symbol "!" $> EBuiltinOp ENot
        , symbol "<" $> EBuiltinOp ELessThan
        , symbol ">" $> EBuiltinOp EGreaterThan
        , symbol "=" $> EBuiltinOp EEqual
        ]

exprAtom :: Parser Expr
exprAtom = choice
    [ symbol "(" *> expr <* symbol ")"
    , try (EVar <$> identifier) -- We try ident, because it could fail reading a reserved keyword
    , cond
    , letParser
    , lambda
    , fix
    , literal
    , builtin
    ]

expr :: Parser Expr
expr = do
    a <- exprAtom
    as <- many exprAtom
    case as of
        [] -> return a
        _  -> return $ EApp a as

decl :: Parser Decl
decl = do
        symbol "def"
        name <- identifier
        funDecl name <|> nameDecl name

    where
        funDecl name = do
            args <- args
            body <- block expr
            return $ FunDecl name args body

        nameDecl name = do
            symbol "="
            Decl name <$> expr


program :: Parser [Decl]
program  = many decl <* eof

parseProgram :: T.Text -> Either String [Decl]
parseProgram input = case runParser program "<stdin>" input of
    Left err  -> Left (errorBundlePretty err)
    Right res -> Right res
