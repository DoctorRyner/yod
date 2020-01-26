module Yod.Parser
    ( module Yod.Parser
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void T.Text

strict :: Parser a -> Parser a
strict = flip (<*) eof

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = Lexer.skipBlockCommentNested "{-" "-}"

scn :: Parser ()
scn = Lexer.space space1 lineComment blockComment

sc :: Parser ()
sc = Lexer.space (void $ some $ char ' ' <|> char '\t') lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = Lexer.symbol sc

nameL :: Parser T.Text
nameL = lexeme $ T.cons <$> lowerChar <*> (T.pack <$> many alphaNumChar)

nameU :: Parser T.Text
nameU = lexeme $ T.cons <$> upperChar <*> (T.pack <$> many alphaNumChar)

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') Lexer.charLiteral

stringLiteral :: Parser T.Text
stringLiteral = char '\"' >> (T.pack <$> manyTill Lexer.charLiteral (char '\"'))

integerLiteral :: Parser Int
integerLiteral = Lexer.signed mempty Lexer.decimal

floatLiteral :: Parser Float
floatLiteral = Lexer.signed mempty Lexer.float

booleanLiteral :: Parser Bool
booleanLiteral = lexeme $ choice
    [ True  <$ string "True"
    , False <$ string "False"
    ]

parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"
