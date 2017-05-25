module Main where

import qualified Data.Text.IO as TIO
import           System.Environment                   (getArgs)
import           Text.Megaparsec                      (parse)

import           Compiler.Rum.Internal.AST
import           Compiler.Rum.Internal.Parser         (progP)
import           Compiler.Rum.Internal.ToString       (progToStr)
import           Compiler.Rum.Interpreter.Rummer      (rumInterpreter)
import           Compiler.Rum.StackMachine.Stacker    (rumStacker)
import           Compiler.Rum.Compiler.Rummer         (rumCompiler)

main :: IO ()
main = do
    progArgs@(opt:cmdArgs) <- getArgs  -- [opt, file] <- getArgs
    case opt of
        "-it" -> run "prog.expr" test
        "-i" -> run (head cmdArgs) rumInterpreter
        "-s" -> run (head cmdArgs) rumStacker
        "-c" -> run (head cmdArgs) (rumCompiler $ head cmdArgs)
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
    rumInterpreter p
    TIO.putStrLn $ progToStr 0 p