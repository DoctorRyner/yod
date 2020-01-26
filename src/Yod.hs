module Yod where

import qualified Data.Text                as T
import           System.Console.Haskeline
import           Text.Megaparsec
import           Yod.Parser
import           Yod.Parser.Expr
import           Yod.Runtime

repl :: IO ()
repl = runInputT defaultSettings process
  where
    process = getInputLine "λ = " >>= \case
        Nothing      -> pure ()
        Just ":quit" -> pure ()
        Just ":q"    -> pure ()
        Just input   -> do
            outputStrLn $ case parse (strict expr) "repl-interpreter" $ T.pack input of
                Right result -> either show show $ runtime result
                Left  err    -> errorBundlePretty err
            process
