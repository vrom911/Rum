{-# LANGUAGE LambdaCase #-}
module Compiler.Rum.StackMachine.Util where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM (fromList, insert, lookup, empty)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String               (fromString)
import qualified Data.Text as T
import           Safe                      (headMay)
import           Text.Read                 (readMaybe)


import Compiler.Rum.Internal.AST
import Compiler.Rum.Internal.Rumlude
import Compiler.Rum.Interpreter.Rumlude
import Compiler.Rum.StackMachine.Structure

-----------------
-- Environment --
-----------------
updateSVars :: Variable -> StackEnvironment ->  StackEnvironment
updateSVars v sEnv@SEnv{..} = sEnv{vars = HM.insert v (head stack) vars}

findSVar :: Variable -> StackEnvironment -> Type
findSVar x SEnv{..} = fromMaybe (error "Couldn't find variable") (HM.lookup x vars)

updatePos :: Int -> StackEnvironment -> StackEnvironment
updatePos i sEnv@SEnv{..} = sEnv{pos = i}

succPos :: StackEnvironment -> StackEnvironment
succPos sEnv@SEnv{..} = sEnv {pos = succ pos}

pushStack :: Type -> StackEnvironment -> StackEnvironment
pushStack el sEnv@SEnv{..} = sEnv{stack = el : stack}

popStack :: StackEnvironment -> StackEnvironment
popStack sEnv@SEnv{..} = case stack of
    [] -> error "Can not pop from empty stack"
    _  -> sEnv{stack = tail stack}

takeStack :: StackEnvironment -> Type
takeStack SEnv{..} = fromMaybe (error "Can not take from empty stack") (headMay stack)

takePopStack :: InterpretStackType
takePopStack = gets takeStack >>= \x -> modify popStack >> return x

popNstack :: Int -> StackEnvironment -> StackEnvironment
popNstack n sEnv@SEnv{..} = sEnv{ stack = drop n stack }

emptyVars :: VarEnv
emptyVars = HM.empty
---------------------
--- Labels' stuff ---
---------------------

buildLabel :: Int -> LabelId
buildLabel n = fromString ("label" ++ show n)

newLabel :: State Int LabelId
newLabel = get >>= \x -> modify (+1) >> return (buildLabel x)

buildLabelsMap :: [Instruction] -> Labels
buildLabelsMap insts = HM.fromList $
    map (\(Label x, i) -> (x, i)) $
        filter (\case {(Label _, _) -> True;  _ -> False}) $
            zip insts [0..]

findLabel ::  LabelId -> Labels -> Int
findLabel l lbls = fromMaybe (error "Label not found") (HM.lookup l lbls)

----------------------
---- Rumlude Funs ----
----------------------

rumludeFunNames :: RumludeFunNamesMap
rumludeFunNames = HM.fromList [ ("read"  , SRead ), ("write" , SWrite)
                              , ("strlen", Strlen), ("strget", Strget)
                              , ("strsub", Strsub), ("strdup", Strdup)
                              , ("strset", Strset), ("strcat", Strcat)
                              , ("strcmp", Strcmp), ("strmake", Strmake)
                              ]
rumludeFunArgs = HM.fromList [ (Strlen, 1), (Strget, 2)
                             , (Strsub, 3), (Strdup, 1)
                             , (Strset, 3), (Strcat, 2)
                             , (Strcmp, 2), (Strmake, 2)
                             ]
executeRumlude :: RumludeFunName -> InterpretStack
executeRumlude SRead = liftIO getLine >>= \input ->
    modify (pushStack $ Number $ fromMaybe (error "Wrong input") (readMaybe input))
executeRumlude SWrite = takePopStack >>= \x -> liftIO $ putStrLn $ typeToInt x
executeRumlude f = replicateM (fromMaybe (error "Something gone wrong") (HM.lookup f rumludeFunArgs)) takePopStack >>=
    \args -> modify (pushStack $ runRumlude f $ reverse args)
--executeRumlude Strlen = takePopStack >>= \(Str s) -> modify (pushStack $ Number $ T.length s)
--executeRumlude Strget = takePopStack >>= \(Str s) ->
--    takePopStack >>= \(Number i) ->
--        modify (pushStack $ Ch $ T.index s i)
--executeRumlude Strsub = takePopStack >>= \(Str s) ->
--    takePopStack >>= \(Number from) ->
--        takePopStack >>= \(Number n) -> modify (pushStack $ Str $ T.take n (T.drop from s))
--executeRumlude Strdup = takePopStack >>= \(Str s) -> modify (pushStack $ Str s)
--executeRumlude Strset = takePopStack >>= \(Str s) ->
--    takePopStack >>= \(Number i) ->
--        takePopStack >>= \(Ch c) -> let (beg, rest) = T.splitAt i s in
--                                modify (pushStack $ Str $ beg <> T.cons c (T.tail rest))
--executeRumlude Strcat = takePopStack >>= \(Str s) ->
--    takePopStack >>= \(Str d) -> modify (pushStack $ Str $ s <> d)
--executeRumlude Strcmp = takePopStack >>= \(Str s) ->
--    takePopStack >>= \(Str d) ->
--                            modify (pushStack $ Number $ case compare s d of
--                                EQ -> 0
--                                LT -> -1
--                                GT -> 1 )
--executeRumlude Strmake = takePopStack >>= \(Number n) ->
--    takePopStack >>= \(Ch ch) ->
--        modify (pushStack $ Str $ T.replicate n (T.singleton ch))