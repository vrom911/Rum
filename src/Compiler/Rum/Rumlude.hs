module Compiler.Rum.Rumlude where

import           Control.Applicative       (empty)
import           Control.Monad.Trans       (liftIO)
import qualified Data.HashMap.Strict as HM (fromList)
import           Text.Read                 (readMaybe)


import           Compiler.Rum.Structure
import           Compiler.Rum.ToString (typeToStr)

-----------------------
---- Default Funcs ----
-----------------------

preludeLibrary :: FunEnv
preludeLibrary = HM.fromList [ (Variable "read", ([], readFun))
                             , (Variable "write", ([], writeFun))]
  where
    readFun :: [Type] -> MyStateT
    readFun _ = liftIO getLine >>= \input -> maybe empty (pure . Number) (readMaybe input)

    writeFun :: [Type] -> MyStateT
    writeFun [x] = do
--        liftIO $ putStr "> > " -- for compiler-test/expressions
        --    liftIO $ putStr "> > > > "-- for compiler-test/deep-expressions
        liftIO $ putStrLn $ typeToStr x  -- res ?: error "writeln error"
        return Unit
    writeFun _ = error "Paste Several arggs to write function!"