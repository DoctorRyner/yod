module Yod where

import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.Haskeline
import           Text.Megaparsec
import           Yod.Module
import           Yod.Parser
import           Yod.Parser.Expr
import           Yod.Parser.Module
import           Yod.Runtime

replLoad :: Module -> IO ()
replLoad module_ = do
    runInputT defaultSettings process
  where
    process = getInputLine "λ = " >>= \case
        Nothing      -> pure ()
        Just ":quit" -> pure ()
        Just ":q"    -> pure ()
        Just input   -> do
            outputStrLn $ case parse (strict expr) "repl-interpreter" $ T.pack input of
                Right result -> either show show (runtimeIn module_.env result)
                Left  err    -> errorBundlePretty err
            process

replLoadFile :: FilePath -> IO ()
replLoadFile path = replLoad =<< readModule path

repl :: IO ()
repl = replLoad defaultModule

readModule :: FilePath -> IO Module
readModule path = do
    src <- T.readFile path

    case parse (strict module_) path src of
        Right loadedModule -> do
            putStrLn $ "Loaded module: " ++ path
            print loadedModule
            pure loadedModule
        Left err           -> fail $ errorBundlePretty err
