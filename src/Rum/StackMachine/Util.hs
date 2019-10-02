module Rum.StackMachine.Util
       ( -- * Environment
         updateSmallVars
       , updateSVars
       , findSVar

       , getSArrayCell
       , updateSArrs
       , updatePos
       , succPos

       , pushStack
       , popStack
       , takeStack
       , takePopStack
       , popNstack
       , emptyVars

         -- * Label
       , buildLabel
       , newLabel
       , buildLabelsMap
       , findLabel
       ) where

import Relude.Extra.Enum (next)

import Safe (headMay)

import Rum.Internal.AST (RumType (..), Variable)
import Rum.Internal.Util (getArrsCell, setArrsCell)
import Rum.StackMachine.Structure (Instruction (..), InterpretStackType, LabelId, Labels,
                                   RefSVarEnv, SRefType (..), StackEnvironment (..))

import qualified Data.HashMap.Strict as HM (empty, fromList, insert, lookup)


-----------------
-- Environment --
-----------------
updateSmallVars :: Variable -> StackEnvironment -> StackEnvironment
updateSmallVars v sEnv@SEnv{..} = case viaNonEmpty head stack of
    Just val -> sEnv {vars = HM.insert v (Val val) vars}
    Nothing  -> sEnv

updateSVars :: Variable -> SRefType -> StackEnvironment -> StackEnvironment
updateSVars v val sEnv@SEnv{..} =
    sEnv{vars = HM.insert v val vars}

findSVar :: Variable -> StackEnvironment -> SRefType
findSVar x SEnv{..} = fromMaybe (error "Couldn't find variable") (HM.lookup x vars)


--findSArrCell :: Variable -> [SRefType] -> StackEnvironment -> SRefType
--findSArrCell v inds senv = let fullArr = findSVar v senv in
--    getSArrayCell fullArr inds

getSArrayCell :: SRefType -> [RumType] -> IO RumType
getSArrayCell (RefVal x) ind = do
    rX <- readIORef x
    pure $ getArrsCell rX ind

updateSArrs :: Variable -> [RumType] -> RumType -> StackEnvironment -> IO ()
updateSArrs v inds val env@SEnv{..} = do
    let Just (RefVal arr) = HM.lookup v vars
    rArr <- readIORef arr
    writeIORef arr (Arr (setArrsCell inds val rArr))

updatePos :: Int -> StackEnvironment -> StackEnvironment
updatePos i sEnv@SEnv{..} = sEnv{pos = i}

succPos :: StackEnvironment -> StackEnvironment
succPos sEnv@SEnv{..} = sEnv {pos = next pos}

pushStack :: RumType -> StackEnvironment -> StackEnvironment
pushStack el sEnv@SEnv{..} = sEnv{stack = el : stack}

popStack :: StackEnvironment -> StackEnvironment
popStack sEnv@SEnv{..} = case stack of
    [] -> error "Can not pop from empty stack"
    _  -> sEnv{stack = drop 1 stack}

takeStack :: StackEnvironment -> RumType
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
