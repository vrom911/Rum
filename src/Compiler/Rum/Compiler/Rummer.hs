module Compiler.Rum.Compiler.Rummer where

import Compiler.Rum.Internal.AST
import Compiler.Rum.Compiler.Emitter (codeGenMaybeWorks)
import Compiler.Rum.Compiler.JIT     (runJIT)

rumCompiler :: String -> Program -> IO ()
rumCompiler modName p = do
    m <- codeGenMaybeWorks modName p
    runJIT m >> return ()
