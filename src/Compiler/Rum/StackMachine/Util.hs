{-# LANGUAGE LambdaCase #-}

module Compiler.Rum.StackMachine.Util where

import           Control.Monad.State                 ( State, get, gets, modify )
import qualified Data.HashMap.Strict as HM           ( fromList, insert, lookup, empty )
import           Data.Maybe                          ( fromMaybe )
import           Data.IORef
import           Data.String                         ( fromString )
import           Safe                                ( headMay )

import           Compiler.Rum.Internal.AST           ( Type(..), Variable)
import           Compiler.Rum.Internal.Util
import           Compiler.Rum.StackMachine.Structure ( Instruction(..)
                                                     , InterpretStackType
                                                     , LabelId, Labels
                                                     , RefSVarEnv
                                                     , StackEnvironment(..)
                                                     , SRefType(..)
                                                     )

----------------------------
-------- Environment -------
----------------------------
updateSmallVars :: Variable -> StackEnvironment ->  StackEnvironment
updateSmallVars v sEnv@SEnv{..} =
    sEnv{vars = HM.insert v (Val $ head stack) vars}

updateSVars :: Variable -> SRefType -> StackEnvironment ->  StackEnvironment
updateSVars v val sEnv@SEnv{..} =
    sEnv{vars = HM.insert v val vars}

findSVar :: Variable -> StackEnvironment -> SRefType
findSVar x SEnv{..} = fromMaybe (error "Couldn't find variable") (HM.lookup x vars)

getSArrayCell :: SRefType -> [Type] -> IO Type
getSArrayCell (RefVal x) ind = do
    rX <- readIORef x
    pure $ getArrsCell rX ind
getSArrayCell _  _ = error "getSArrayCell error. Impossible"


updateSArrs :: Variable -> [Type] -> Type -> StackEnvironment -> IO ()
updateSArrs v inds val SEnv{..} = do
    let Just (RefVal arr) = HM.lookup v vars
    rArr <- readIORef arr
    writeIORef arr (Arr (setArrsCell inds val rArr))

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

emptyVars :: RefSVarEnv
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