module Compiler.Rum.StackMachine.Stacker where

import           Control.Monad.State       (execStateT, get, gets, liftIO, modify, replicateM, unless, when)
import           Control.Monad.Trans.Reader(ask, asks, runReaderT)
import qualified Data.HashMap.Strict as HM (lookup)
import           Data.Maybe                (fromMaybe)
import           Text.Read                 (readMaybe)

import Compiler.Rum.Internal.AST
import Compiler.Rum.Internal.Util
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
    executeInstr (Push x)   = push x
    executeInstr Pop        = pop
    executeInstr (PushNArr n) = replicateM n takePopStack >>= \arr -> push (Arr $ reverse arr)
    executeInstr (LoadArr ar n) = replicateM n takePopStack >>= \indexes ->
        gets (findSArrCell ar (reverse indexes)) >>= push
    executeInstr (StoreArr ar n) = replicateM n takePopStack >>= \indexes ->
        takePopStack >>= \val -> modify (updateSArrs ar (reverse indexes) val)
    executeInstr (SBinOp b) =
        gets takeStack >>= \(Number y) ->
            pop >> gets takeStack >>= \(Number x) ->
                pop >> push (Number (binOp b x y))
    executeInstr (SLogicOp l) =
        gets takeStack >>= \(Number y) ->
            pop >> gets takeStack >>= \(Number x) ->
                pop >> push (Number (logicOp l x y))
    executeInstr (SCompOp c)  =
        gets takeStack >>= \y ->
            pop >> gets takeStack >>= \x ->
                pop >> push (intCompare c x y)
    executeInstr (Load v)        = gets (findSVar v) >>= push
    executeInstr (Store v)       = modify (updateSVars v) >> pop
    executeInstr (Jump l)        = asks snd >>= \lbls -> modify (updatePos (findLabel l lbls))
    executeInstr (JumpIfFalse l) = gets takeStack >>= \s -> when (isFalse s) $ executeInstr (Jump l)
    executeInstr (JumpIfTrue l)  = gets takeStack >>= \s -> unless (isFalse s) $ executeInstr (Jump l)
    executeInstr SReturn         = return ()
    executeInstr (SFunCall f n)  = ask >>= \(instrs, lbls) ->
        get >>= \SEnv{..} ->
           liftIO (execStateT  (runReaderT execute (instrs, lbls)) (SEnv emptyVars (take n stack) (findLabel f lbls))) >>=
           \newEnv -> modify (popNstack n) >> push (takeStack newEnv)
    executeInstr (SRumludeCall f) = executeRumlude f


    executeRumlude :: RumludeFunName -> InterpretStack
    executeRumlude Read = liftIO getLine >>= \input ->
        modify (pushStack $ Number $ fromMaybe (error "Wrong input") (readMaybe input))
    executeRumlude Write = takePopStack >>= writeRumlude
    executeRumlude f = replicateM (fromMaybe (error "Something gone wrong") (HM.lookup f rumludeFunArgs)) takePopStack >>=
        \args -> push (runRumlude f $ reverse args)

    push :: Type -> InterpretStack
    push = modify . pushStack
    pop :: InterpretStack
    pop = modify popStack
