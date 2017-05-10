module Main where

import Control.Monad.State
import System.Environment   (getArgs)
import Text.Megaparsec      (parse)


import Compiler.Rum.Interpreter
import Compiler.Rum.Parser
import Compiler.Rum.Structure (Statement)
import Compiler.Rum.ToString

main :: IO ()
main = do
    l@(opt:args) <- getArgs  -- [opt, file] <- getArgs
    case opt of
        "-t" -> run "prog.expr" test
        "-i" -> run (head args) interpr
        _    -> print l

run :: String -> ([Statement] -> IO ()) -> IO ()
run fileName f = do
    prog <- readFile fileName
    let statements = parse progP "" prog
    case statements of
        Left err -> error (show err)
        Right p  -> f p

test :: [Statement] -> IO ()
test p = do
    print p
    evalStateT (interpret p) mempty
    putStrLn $ progToStr 0 p

interpr :: [Statement] -> IO ()
interpr p = evalStateT (interpret p) mempty