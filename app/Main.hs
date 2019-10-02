module Main (main) where

import System.Environment (getArgs)
import Text.Megaparsec (parse)

import Rum.Compiler.Rummer (rumCompiler)
import Rum.Internal.AST (Statement)
import Rum.Internal.Parser (progP)
import Rum.Internal.ToString (progToStr)
import Rum.Interpreter.Rummer (rumInterpreter)
import Rum.StackMachine.Stacker (rumStacker)


main :: IO ()
main = getArgs >>= \case
    [] -> putStrLn "Please specify an option"
    progArgs@(opt:cmdArgs) ->
        if opt == "-it"
        then run "prog.expr" test
        else case cmdArgs of
            [] -> putStrLn "Incorrect options specified"
            nm:_ -> case opt of
                "-i" -> run nm rumInterpreter
                "-s" -> run nm rumStacker
                "-c" -> run nm (rumCompiler nm)
                _    -> print progArgs

run :: String -> ([Statement] -> IO ()) -> IO ()
run fileName f = do
    prog <- readFileText fileName
    let statements = parse progP "" prog
    case statements of
        Left err -> error (show err)
        Right p  -> f p

test :: [Statement] -> IO ()
test p = do
    print p
    rumInterpreter p
    putTextLn $ progToStr 0 p
