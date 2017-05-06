module Main where

import Control.Monad.Reader
import Text.Megaparsec      (parse)

import Compiler.Rum.Interpreter
import Compiler.Rum.Parser
import Compiler.Rum.ToString

main :: IO ()
main = do
    prog <- readFile "prog.expr"
    let statements = parse progP "" prog
    case statements of
        Left err -> error (show err)
        Right p  -> do
            print p
            print $ runReader (interpret p) mempty
            putStrLn $ progToStr 0 p
