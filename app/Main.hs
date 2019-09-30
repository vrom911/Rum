module Main (main) where

import System.Environment (getArgs)
import Text.Megaparsec (parse)

import Rum.Compiler.Rummer (rumCompiler)
import Rum.Internal.AST (Statement)
import Rum.Internal.Parser (progP)
import Rum.Internal.ToString (progToStr)
import Rum.Interpreter.Rummer (rumInterpreter)
import Rum.StackMachine.Stacker (rumStacker)

import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    progArgs@(opt:cmdArgs) <- getArgs
    case opt of
        "-it" -> run "prog.expr" test
        "-i"  -> run (head cmdArgs) rumInterpreter
        "-s"  -> run (head cmdArgs) rumStacker
        "-c"  -> run (head cmdArgs) (rumCompiler $ head cmdArgs)
        _     -> print progArgs

run :: String -> ([Statement] -> IO ()) -> IO ()
run fileName f = do
    prog <- TIO.readFile fileName
    let statements = parse progP "" prog
    case statements of
        Left err -> error (show err)
        Right p  -> f p

test :: [Statement] -> IO ()
test p = do
    print p
    rumInterpreter p
    TIO.putStrLn $ progToStr 0 p
