
module Yod.Expr where

import qualified Data.Map  as Dict
import qualified Data.Text as T
import           Yod.Type  (Type)

type Env = Dict.Map T.Text Expr

data Value
    = Int      Int
    | Bool     Bool
    | Char     Char
    | Float    Float
    | String   T.Text
    | Closure (T.Text, Type) Expr Env
    deriving Show

data Expr
    = Value  Value
    | Var    T.Text
    | Apply  Expr Expr
    | Lambda (T.Text, Type) Expr
    | If Expr Expr Expr
    | LetIn Env Expr
    deriving Show
