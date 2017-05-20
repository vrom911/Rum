module Compiler.Rum.Interpreter.Rumlude where

import           Control.Applicative       (empty)
import           Control.Monad.Trans       (liftIO)
import qualified Data.HashMap.Strict as HM (fromList)
import           Text.Read                 (readMaybe)

import           Compiler.Rum.Internal.AST
import           Compiler.Rum.Internal.Rumlude

-----------------------
---- Default Funcs ----
-----------------------

preludeLibrary :: FunEnv
preludeLibrary = HM.fromList [ ("read",    ([], readFun))
                             , ("write",   ([], writeFun))
                             , ("strlen",  ([], interpretRumlude Strlen))
                             , ("strget",  ([], interpretRumlude Strget))
                             , ("strsub",  ([], interpretRumlude Strsub))
                             , ("strdup",  ([], interpretRumlude Strdup))
                             , ("strset",  ([], interpretRumlude Strset))
                             , ("strcat",  ([], interpretRumlude Strcat))
                             , ("strcmp",  ([], interpretRumlude Strcmp))
                             , ("strmake", ([], interpretRumlude Strmake))
                             , ("arrlen",  ([], interpretRumlude Arrlen))
                             , ("arrmake", ([], interpretRumlude Arrmake))
                             , ("Arrmake", ([], interpretRumlude Arrmake))
                             ]
  where
    readFun :: [Type] -> InterpretT
    readFun _ = liftIO getLine >>= \input -> maybe empty (pure . Number) (readMaybe input)

    writeFun :: [Type] -> InterpretT
    writeFun [x] = Unit <$ writeRumlude x
    writeFun _   = error "Paste Several arggs to write function"

    ----------------------
    -- String Functions --
    ----------------------
    interpretRumlude :: RumludeFunName -> [Type] -> InterpretT
    interpretRumlude f = return . runRumlude f