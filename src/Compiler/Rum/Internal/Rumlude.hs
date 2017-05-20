module Compiler.Rum.Internal.Rumlude where

import           Data.Monoid ((<>))
import qualified Data.Text as T


import Compiler.Rum.Internal.AST
import Compiler.Rum.StackMachine.Structure

runRumlude :: RumludeFunName -> [Type] -> Type
----------------------
-- String Functions --
----------------------
runRumlude Strlen [Str s] = Number $ T.length s
runRumlude Strget [Str s, Number i] = Ch $ T.index s i
runRumlude Strsub [Str s, Number from, Number n] = Str (T.take n (T.drop from s))
runRumlude Strdup [Str s] = Str s
runRumlude Strset [Str s, Number i, Ch c] =
    let (beg, rest) = T.splitAt i s in
    Str $ beg <> T.cons c (T.tail rest)
runRumlude Strcat [Str s, Str d] = Str $ s <> d
runRumlude Strcmp [Str s, Str d] = Number $ case compare s d of
    EQ -> 0
    LT -> -1
    GT -> 1
runRumlude Strmake [Number n, Ch ch] = Str $ T.replicate n (T.singleton ch)
----------------------
-- Array Functions --
----------------------
runRumlude Arrlen [Arr ar] = Number $ length ar
runRumlude Arrmake [Number n, x] = Arr $ replicate n x

runRumlude f args = error $ "Incorrect arguments in " <> show f <> "(" <> show args <> ")"