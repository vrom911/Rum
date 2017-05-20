module Compiler.Rum.StackMachine.Stacker where

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM (lookup)
import           Data.Maybe                (fromMaybe)
import           Text.Read                 (readMaybe)

import Compiler.Rum.Internal.AST
import Compiler.Rum.Internal.Rumlude
import Compiler.Rum.StackMachine.Structure
import Compiler.Rum.StackMachine.Util

stacker :: InterpretStack
stacker = startPosition >> execute

startPosition :: InterpretStack
startPosition = asks snd >>= \lbls -> modify (updatePos $ findLabel "start" lbls)

execute :: InterpretStack
execute = do
    instrs <- asks fst
    curPos <- gets pos
    let curInstr = instrs !! curPos
    when (curPos < length instrs && not (isReturn curInstr)) $ do
        executeInstr curInstr
        modify succPos
        execute
  where
    isReturn :: Instruction -> Bool
    isReturn SReturn = True
    isReturn _ = False

    executeInstr :: Instruction -> InterpretStack
    executeInstr Nop        = return ()
    executeInstr (Label _)  = return ()
    executeInstr (Push x)   = modify (pushStack x)
    executeInstr Pop        = modify popStack
    executeInstr (PushNArr n) = replicateM n takePopStack >>= \arr -> modify (pushStack $ Arr (reverse arr))
    executeInstr (LoadArr ar n) = replicateM n takePopStack >>= \indexes ->
        gets (findSArrCell ar (reverse indexes)) >>= \res ->
            modify (pushStack res)
    executeInstr (StoreArr ar n) = replicateM n takePopStack >>= \indexes ->
        takePopStack >>= \val -> modify (updateSArrs ar (reverse indexes) val)
    executeInstr (SBinOp b) =
        gets takeStack >>= \(Number y) ->
            modify popStack >> gets takeStack >>= \(Number x) ->
                modify popStack >> modify (pushStack $ Number (binOp b x y))
    executeInstr (SLogicOp l) =
        gets takeStack >>= \(Number y) ->
            modify popStack >> gets takeStack >>= \(Number x) ->
                modify popStack >> modify (pushStack $ Number (logicOp l x y))
    executeInstr (SCompOp c)  =
        gets takeStack >>= \y ->
            modify popStack >> gets takeStack >>= \x ->
                modify popStack >> modify (pushStack (intCompare c x y))
    executeInstr (Load v)        = gets (findSVar v) >>= modify . pushStack
    executeInstr (Store v)       = modify (updateSVars v) >> modify popStack
    executeInstr (Jump l)        = asks snd >>= \lbls -> modify (updatePos (findLabel l lbls))
    executeInstr (JumpIfFalse l) = get >>= \s -> when (takeStack s == Number 0) $ executeInstr (Jump l)
    executeInstr (JumpIfTrue l)  = get >>= \s -> when (takeStack s /= Number 0) $ executeInstr (Jump l)
    executeInstr SReturn         = return ()
    executeInstr (SFunCall f n)  = ask >>= \(instrs, lbls) ->
        get >>= \SEnv{..} ->
           liftIO (execStateT  (runReaderT execute (instrs, lbls)) (SEnv emptyVars (take n stack) (findLabel f lbls))) >>=
           \newEnv -> modify (popNstack n) >> modify (pushStack $ takeStack newEnv)
    executeInstr (SRumludeCall f) = executeRumlude f


    executeRumlude :: RumludeFunName -> InterpretStack
    executeRumlude Read = liftIO getLine >>= \input ->
        modify (pushStack $ Number $ fromMaybe (error "Wrong input") (readMaybe input))
    executeRumlude Write = takePopStack >>= writeRumlude
    executeRumlude f = replicateM (fromMaybe (error "Something gone wrong") (HM.lookup f rumludeFunArgs)) takePopStack >>=
        \args -> modify (pushStack $ runRumlude f $ reverse args)

    -- TODO: modify pushStack == push
    -- TODO: modify popStack  == pop