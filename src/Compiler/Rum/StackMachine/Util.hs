{-# LANGUAGE LambdaCase #-}
module Compiler.Rum.StackMachine.Util where

import           Control.Monad.State                 ( State, get, gets, modify )
import qualified Data.HashMap.Strict as HM           ( fromList, insert, lookup, empty )
import           Data.Maybe                          ( fromMaybe )
import           Data.String                         ( fromString )
import           Safe                                ( headMay )

import           Compiler.Rum.Internal.AST           ( Type(..), Variable, VarEnv)
import           Compiler.Rum.Internal.Util          ( setArrsCell )
import           Compiler.Rum.StackMachine.Structure ( Instruction(..)
                                                     , InterpretStackType
                                                     , LabelId, Labels
                                                     , StackEnvironment(..)
                                                     )

-----------------
-- Environment --
-----------------
updateSVars :: Variable -> StackEnvironment ->  StackEnvironment
updateSVars v sEnv@SEnv{..} = sEnv{vars = HM.insert v (head stack) vars}

findSVar :: Variable -> StackEnvironment -> Type
findSVar x SEnv{..} = fromMaybe (error "Couldn't find variable") (HM.lookup x vars)

findSArrCell :: Variable -> [Type] -> StackEnvironment -> Type
findSArrCell v inds senv = let fullArr = findSVar v senv in
    case fullArr of
        Arr _ -> getArrayCell fullArr inds
        _ -> error " Something went wrong with Arrays"

getArrayCell :: Type -> [Type] -> Type
getArrayCell x [] = x
getArrayCell (Arr a) (Number x:xs) = getArrayCell (a !! x) xs
getArrayCell _ _ = error "Error in array finding"

updateSArrs :: Variable -> [Type] -> Type -> StackEnvironment ->  StackEnvironment
updateSArrs v inds val env@SEnv{..} =
    let Just arr = HM.lookup v vars in
    env {vars = HM.insert v (Arr (setArrsCell inds val arr)) vars}

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