module Compiler.Rum.Rumlude where

import           Control.Applicative       (empty)
import           Control.Monad.Trans       (liftIO)
import           Data.Char                 (ord)
import qualified Data.HashMap.Strict as HM (fromList)
import           Data.List                 (intercalate)
import           Text.Read                 (readMaybe)


import           Compiler.Rum.Structure

-----------------------
---- Default Funcs ----
-----------------------

preludeLibrary :: FunEnv
preludeLibrary = HM.fromList [ (Variable "read",    ([], readFun))
                             , (Variable "write",   ([], writeFun))
                             , (Variable "strlen",  ([], strlen))
                             , (Variable "strget",  ([], strget))
                             , (Variable "strsub",  ([], strsub))
                             , (Variable "strdup",  ([], strdup))
                             , (Variable "strset",  ([], strset))
                             , (Variable "strcat",  ([], strcat))
                             , (Variable "strcmp",  ([], strcmp))
                             , (Variable "strmake", ([], strmake))
                             , (Variable "arrlen",  ([], arrlen))
                             , (Variable "arrmake", ([], arrmake))
                             , (Variable "Arrmake", ([], arrmake))
                             ]
  where
    readFun :: [Type] -> MyStateT
    readFun _ = liftIO getLine >>= \input -> maybe empty (pure . Number) (readMaybe input)

    writeFun :: [Type] -> MyStateT
    writeFun [x] = do
--        liftIO $ putStr "> > " -- for compiler-test/expressions
--        liftIO $ putStr "> > > > "-- for compiler-test/deep-expressions
        liftIO $ putStrLn $ typeToInt x  -- res ?: error "writeln error"
        return Unit
          where
            typeToInt :: Type -> String
            typeToInt (Number n) = show n
            typeToInt (Ch c)     = show $ ord c
            typeToInt (Str s)    = s
            typeToInt (Arr ar)   = "[" ++ intercalate ", " (map typeToInt ar) ++ "]"
            typeToInt Unit       = "()"
    writeFun _ = error "Paste Several arggs to write function"

    ----------------------
    -- String Functions --
    ----------------------
    strlen :: [Type] -> MyStateT
    strlen [Str s] = return (Number $ length s)
    strlen _       = error "strlen() can be only applied to Strings"

    strget :: [Type] -> MyStateT
    strget [Str s, Number i] = return (Ch $ s !! i)
    strget _                 = error "strget() params should be (String, Int)"

    strsub :: [Type] -> MyStateT
    strsub [Str s, Number from, Number n] = return (Str (take n (drop from s)))
    strsub _ = error "strsub() params should be (String, Int, Int)"

    strdup :: [Type] -> MyStateT
    strdup [Str s] = return $ Str s
    strdup _       = error "strdup() can be only applied to Strings"

    strset :: [Type] -> MyStateT
    strset [Str s, Number i, Ch c] =
        let (beg, _:rest) = splitAt i s in
        return (Str $ beg ++ (c:rest))
    strset _ = error "strset() params should be (String, Int, Char)"

    strcat :: [Type] -> MyStateT
    strcat [Str s, Str d] = return (Str $ s ++ d)
    strcat _ = error "strcat() can be only applied to Strings"

    strcmp :: [Type] -> MyStateT
    strcmp [Str s, Str d] = return $ Number $ case compare s d of
        EQ -> 0
        LT -> -1
        GT -> 1
    strcmp _ = error "strcmp() can be only applied to Strings"

    strmake :: [Type] -> MyStateT
    strmake [Number n, Ch ch] = return $ Str $ replicate n ch
    strmake _ = error "strmake() errorparams should be (Int, Char)"

    ----------------------
    -- Array Functions --
    ----------------------
    arrlen :: [Type] -> MyStateT
    arrlen [Arr ar] = return (Number $ length ar)
    arrlen _       = error "arrlen() can be only applied to Arrays"

    arrmake :: [Type] -> MyStateT
    arrmake [Number n, x] = return $ Arr $ replicate n x
    arrmake _       = error "arrmake() wrong params"


