module Yod.Parser.Module where

import           Control.Monad.Except
import           Data.Either          (rights)
import qualified Data.Map             as Dict
import qualified Data.Text            as T
import           Yod.Expr
import           Yod.Module
import           Yod.Parser
import           Yod.Parser.Expr
import           Yod.Parser.Type
import           Yod.Type             (Type)

module_ :: Parser Module
module_ = do
    name <- symbol "module" *> nameU
    some newline

    funcs <- some func
    let env = Dict.fromList $ rights $ map (runExcept . funcToVar) funcs

    pure $ Module name env

closureFromVars :: Expr -> [(T.Text, Type)] -> Expr
closureFromVars body = \case
    []     -> body
    var:xs -> Lambda var $ closureFromVars body xs


funcToVar :: Statement -> Except T.Text (T.Text, Expr)
funcToVar = \case
    Func name argTypes argNames body -> do
        pure
            ( name
            , closureFromVars body $ zip argNames $ take (length argTypes - 1) argTypes
            )

func :: Parser Statement
func = do
    name <- nameL

    symbol "::"

    types <- type_ `sepBy1` symbol " -> "
    some newline

    symbol name

    argNames <- nameL `sepBy` scn

    symbol "="

    body <- expr

    some newline

    let argNamesCount = length argNames
        argTypesCount = length types - 1
    if argNamesCount == argTypesCount
    then pure $ Func name types argNames body
    else fail $ concat
        [ "In a function " ++ show name ++ " amount of arguments types specified is "
        , show argTypesCount ++ " but we need " ++ show argNamesCount
        ]
