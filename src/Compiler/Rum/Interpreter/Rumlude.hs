module Compiler.Rum.Interpreter.Rumlude where

import           Control.Applicative       (empty)
import           Control.Monad.Trans       (liftIO)
import           Data.Char                 (ord)
import qualified Data.HashMap.Strict as HM (fromList)
import           Data.List                 (intercalate)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Text.Read                 (readMaybe)


import           Compiler.Rum.Internal.AST

-----------------------
---- Default Funcs ----
-----------------------

preludeLibrary :: FunEnv
preludeLibrary = HM.fromList [ ("read",    ([], readFun))
                             , ("write",   ([], writeFun))
                             , ("strlen",  ([], strlen))
                             , ("strget",  ([], strget))
                             , ("strsub",  ([], strsub))
                             , ("strdup",  ([], strdup))
                             , ("strset",  ([], strset))
                             , ("strcat",  ([], strcat))
                             , ("strcmp",  ([], strcmp))
                             , ("strmake", ([], strmake))
                             , ("arrlen",  ([], arrlen))
                             , ("arrmake", ([], arrmake))
                             , ("Arrmake", ([], arrmake))
                             ]
  where
    readFun :: [Type] -> InterpretT
    readFun _ = liftIO getLine >>= \input -> maybe empty (pure . Number) (readMaybe input)

    writeFun :: [Type] -> InterpretT
    writeFun [x] = do
--        liftIO $ putStr "> > " -- for compiler-test/expressions
--        liftIO $ putStr "> > > > "-- for compiler-test/deep-expressions
        liftIO $ putStrLn $ typeToInt x  -- res ?: error "writeln error"
        return Unit
          where
            typeToInt :: Type -> String
            typeToInt (Number n) = show n
            typeToInt (Ch c)     = show $ ord c
            typeToInt (Str s)    = T.unpack s
            typeToInt (Arr ar)   = "[" ++ intercalate ", " (map typeToInt ar) ++ "]"
            typeToInt Unit       = "()"
    writeFun _ = error "Paste Several arggs to write function"

    ----------------------
    -- String Functions --
    ----------------------
    strlen :: [Type] -> InterpretT
    strlen [Str s] = return (Number $ T.length s)
    strlen _       = error "strlen() can be only applied to Strings"

    strget :: [Type] -> InterpretT
    strget [Str s, Number i] = return (Ch $ T.index s i)
    strget _                 = error "strget() params should be (String, Int)"

    strsub :: [Type] -> InterpretT
    strsub [Str s, Number from, Number n] = return (Str (T.take n (T.drop from s)))
    strsub _ = error "strsub() params should be (String, Int, Int)"

    strdup :: [Type] -> InterpretT
    strdup [Str s] = return $ Str s
    strdup _       = error "strdup() can be only applied to Strings"

    strset :: [Type] -> InterpretT
    strset [Str s, Number i, Ch c] =
        let (beg, rest) = T.splitAt i s in
        return (Str $ beg <> T.cons c (T.tail rest))
    strset _ = error "strset() params should be (String, Int, Char)"

    strcat :: [Type] -> InterpretT
    strcat [Str s, Str d] = return (Str $ s <> d)
    strcat _ = error "strcat() can be only applied to Strings"

    strcmp :: [Type] -> InterpretT
    strcmp [Str s, Str d] = return $ Number $ case compare s d of
        EQ -> 0
        LT -> -1
        GT -> 1
    strcmp _ = error "strcmp() can be only applied to Strings"

    strmake :: [Type] -> InterpretT
    strmake [Number n, Ch ch] = return $ Str $ T.replicate n (T.singleton ch)
    strmake _ = error "strmake() error: params should be (Int, Char)"

    ----------------------
    -- Array Functions --
    ----------------------
    arrlen :: [Type] -> InterpretT
    arrlen [Arr ar] = return (Number $ length ar)
    arrlen _       = error "arrlen() can be only applied to Arrays"

    arrmake :: [Type] -> InterpretT
    arrmake [Number n, x] = return $ Arr $ replicate n x
    arrmake _       = error "arrmake() wrong params"


