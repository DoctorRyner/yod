module Yod.Module where

import qualified Data.Text as T
import           Yod.Expr
import           Yod.Type

data Module = Module
    { name :: T.Text
    , env  :: Env
    }
    deriving Show

defaultModule :: Module
defaultModule = Module "repl-interpreter" mempty

data Statement
    = Func { name :: T.Text, types :: [Type], argNames :: [T.Text], body :: Expr }
    deriving Show
