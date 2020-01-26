module Yod.Parser.Expr where

import qualified Data.Map        as Dict
import qualified Data.Text       as T
import           Yod.Expr
import           Yod.Parser
import           Yod.Parser.Type (type_)

term :: Parser Expr
term = choice
    [ parens expr
    , Value . String <$> stringLiteral
    , Value . Char <$> charLiteral
    , Value . Float <$> try floatLiteral
    , Value . Int <$> integerLiteral
    , Value . Bool <$> booleanLiteral
    , lambda
    , letIn
    , if_
    , try var
    ]

expr :: Parser Expr
expr = foldl1 Apply <$> sepEndBy1 term sc

if_ :: Parser Expr
if_ = do
    symbol "if"
    condition <- expr
    thenExpr  <- between (symbol "then") (symbol "else") expr
    If condition thenExpr <$> expr

reservedWords :: [T.Text]
reservedWords =
    [ "if"
    , "then"
    , "else"
    , "let"
    , "in"
    ]

var :: Parser Expr
var = Var <$> (try . check =<< nameL)
  where
    check word =
        if word `elem` reservedWords
        then fail $ show $ "keyword: `" <> word <> "` is a reserved word"
        else pure word

lambda :: Parser Expr
lambda = do
    symbol "\\"
    types <-
        sepBy1
            (do
                argName <- nameL
                symbol "::"
                type_   <- type_
                pure (argName, type_)
            )
            $ symbol ","
    sc
    symbol "->"
    body <- expr

    pure $ foldr Lambda body types

letIn :: Parser Expr
letIn = do
    symbol "let"
    lets <-
        sepBy1
            (do
                name <- nameL
                symbol "="
                body <- expr
                pure (name, body)
            )
            $ symbol ","

    symbol "in"

    let env = Dict.fromList lets

    LetIn env <$> expr
