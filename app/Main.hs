module Main where

import System.Environment        (getArgs)
import Text.Megaparsec           (parse)

import Compiler.Rum.Interpreter
import Compiler.Rum.Parser
import Compiler.Rum.Rumlude
import Compiler.Rum.Structure
import Compiler.Rum.ToString

main :: IO ()
main = do
    progArgs@(opt:cmdArgs) <- getArgs  -- [opt, file] <- getArgs
    case opt of
        "-t" -> run "prog.expr" test
        "-i" -> run (head cmdArgs) interpr
        _    -> print progArgs

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
    interpr p
    putStrLn $ progToStr 0 p

interpr :: [Statement] -> IO ()
interpr p = runIOInterpret (interpret p) (Env mempty preludeLibrary False)