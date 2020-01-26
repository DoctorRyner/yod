module Yod.Parser.Type where

import           Yod.Parser
import           Yod.Type

type_ :: Parser Type
type_ = choice
    [ parens type_
    , Bool <$ "Bool"
    , Int <$ "Int"
    , Float <$ "Float"
    , Char <$ "Char"
    , String <$ "String"
    ]

-- lambda :: Parser Type
-- lambda = do
    -- let types = sepBy1 type_ "->"
--
    -- pure Bool
