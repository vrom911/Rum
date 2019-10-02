module Rum.Internal.Rumlude
       ( runRumlude
       , writeRumlude

       , rumludeFunNames
       , rumludeFunArgs
       ) where

import Data.Char (ord)

import Rum.Internal.AST (RumType (..), RumludeFunName (..))
import Rum.StackMachine.Structure (RumludeFunNamesMap)

import qualified Data.HashMap.Strict as HM (HashMap, fromList)
import qualified Data.Text as T


rumludeFunNames :: RumludeFunNamesMap
rumludeFunNames = HM.fromList
    [ ("read"  , Read )
    , ("write" , Write)
    , ("strlen", Strlen)
    , ("strget", Strget)
    , ("strsub", Strsub)
    , ("strdup", Strdup)
    , ("strset", Strset)
    , ("strcat", Strcat)
    , ("strcmp", Strcmp)
    , ("strmake", Strmake)
    , ("arrlen", Arrlen)
    , ("arrmake", Arrmake)
    , ("Arrmake", Arrmake)
    ]

rumludeFunArgs :: HM.HashMap RumludeFunName Int
rumludeFunArgs = HM.fromList
    [ (Strlen, 1)
    , (Strget, 2)
    , (Strsub, 3)
    , (Strdup, 1)
    , (Strset, 3)
    , (Strcat, 2)
    , (Strcmp, 2)
    , (Strmake, 2)
    , (Arrlen, 1)
    , (Arrmake, 2)
    ]

runRumlude :: RumludeFunName -> [RumType] -> RumType
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
runRumlude Strmake [Number n, Ch ch] = Str $ T.replicate n (one ch)
----------------------
-- Array Functions --
----------------------
runRumlude Arrlen  [Arr ar] = Number $ length ar
runRumlude Arrmake [Number n, x] = Arr $ replicate n x

runRumlude f args = error $ "Incorrect arguments in " <> show f <> "(" <> show args <> ")"


typeToInt :: RumType -> String
typeToInt (Number n) = show n
typeToInt (Ch c)     = show $ ord c
typeToInt (Str s)    = toString s
typeToInt (Arr ar)   = "[" ++ intercalate ", " (map typeToInt ar) ++ "]"
typeToInt Unit       = "()"

writeRumlude :: MonadIO m => RumType -> m ()
writeRumlude x =
--    liftIO $ putStr "> > " -- for compiler-test/expressions
--    liftIO $ putStr "> > > > "-- for compiler-test/deep-expressions
    liftIO $ putStrLn $ typeToInt x  -- res ?: error "writeln error"
