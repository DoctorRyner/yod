module Yod.Type where

import qualified Data.Text as T

data Type
    = Int
    | Float
    | Bool
    | Char
    | String
    | Lambda Type Type
    deriving (Show, Eq)

data Error
    = ApplyingRequiresRuntimeEvaluation
    | UnknownIdentifier T.Text
    | Custom T.Text
    | NotAFunction T.Text
    | Missmatch Type Type
    deriving Show

typeToText :: Type -> T.Text
typeToText = \case
    Lambda argType bodyType -> "(" <> typeToText argType <> " -> " <> typeToText bodyType <> ")"
    otherTypes              -> T.pack $ show otherTypes
