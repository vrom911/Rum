module Rum.Compiler.Rummer
       ( rumCompiler
       ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import Rum.Compiler.Emitter (codeGenMaybeWorks)
import Rum.Compiler.JIT (runJIT)
import Rum.Internal.AST


rumCompiler :: String -> Program -> IO ()
rumCompiler modName p = do
    m <- codeGenMaybeWorks (toShort $ pack modName) p
    () <$ runJIT m
