module Main where

import Control.Monad.State
import Control.Monad.Trans.Reader
import qualified Data.Text.IO as TIO
import System.Environment        (getArgs)
import Text.Megaparsec           (parse)

import Compiler.Rum.Interpreter.Rummer
import Compiler.Rum.Internal.Parser
import Compiler.Rum.Interpreter.Rumlude
import Compiler.Rum.Internal.AST
import Compiler.Rum.Internal.ToString
import Compiler.Rum.StackMachine.Translator
import Compiler.Rum.StackMachine.Stacker
import Compiler.Rum.StackMachine.Structure
import Compiler.Rum.StackMachine.Util

main :: IO ()
main = do
    progArgs@(opt:cmdArgs) <- getArgs  -- [opt, file] <- getArgs
    case opt of
        "-it" -> run "prog.expr" test
        "-st" -> run "prog.expr" stackTest
        "-i" -> run (head cmdArgs) interpr
        "-s" -> run (head cmdArgs) stackRun
        _    -> print progArgs

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
    interpr p
    TIO.putStrLn $ progToStr 0 p

interpr :: [Statement] -> IO ()
interpr p = runIOInterpret (interpret p) (Env mempty preludeLibrary False)

stackRun :: Program -> IO ()
stackRun p = do
    let instrs = evalState (translateP p) 0
    evalStateT (runReaderT stacker (instrs, buildLabelsMap instrs)) (SEnv emptyVars [] 0)

stackTest :: Program -> IO ()
stackTest p = do
    print p
    let instrs = evalState (translateP p) 0
    print instrs
    evalStateT (runReaderT stacker (instrs, buildLabelsMap instrs)) (SEnv emptyVars [] 0)