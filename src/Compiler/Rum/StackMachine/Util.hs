{-# LANGUAGE LambdaCase #-}
module Compiler.Rum.StackMachine.Util where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM (fromList, insert, lookup, empty)
import           Data.Maybe                (fromMaybe)
import           Data.String               (fromString)
import           Safe                      (headMay)
import           Text.Read                 (readMaybe)


import Compiler.Rum.Internal.AST
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
rumludeFunNames = HM.fromList [("read", SRead), ("write", SWrite)]

executeRumlude :: RumludeFunName -> InterpretStack
executeRumlude SRead = liftIO getLine >>= \input ->
    modify (pushStack $ Number $ fromMaybe (error "Wrong input") (readMaybe input))
executeRumlude SWrite = gets takeStack >>= \x -> liftIO $ putStrLn $ typeToInt x