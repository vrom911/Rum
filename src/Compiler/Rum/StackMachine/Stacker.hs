module Compiler.Rum.StackMachine.Stacker where

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.Bool                 (bool)

import Compiler.Rum.Internal.AST
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
    executeInstr (Push x)   = modify (pushStack x) >> pure ()
    executeInstr Pop        = modify popStack >> pure ()
    executeInstr (SBinOp b) =
        gets takeStack >>= \(Number y) ->
            modify popStack >> gets takeStack >>= \(Number x) ->
                modify popStack >> modify (pushStack $ Number (binOp b x y)) >> return ()
    executeInstr (SLogicOp l) =
        gets takeStack >>= \(Number y) ->
            modify popStack >> gets takeStack >>= \(Number x) ->
                modify popStack >> modify (pushStack $ Number (logicOp l x y)) >> return ()
    executeInstr (SCompOp c)  =
        gets takeStack >>= \y ->
            modify popStack >> gets takeStack >>= \x ->
                modify popStack >> modify (pushStack (intCompare x y)) >> return ()
      where
        intCompare :: Type -> Type -> Type
        intCompare x y = Number $ bool 0 1 $ compOp c x y
    executeInstr (Load v)        = gets (findSVar v) >>= \x -> void (modify (pushStack x))
    executeInstr (Store v)       = modify (updateSVars v) >> modify popStack >> return ()
    executeInstr (Jump l)        = asks snd >>= \lbls -> modify (updatePos (findLabel l lbls))
    executeInstr (JumpIfFalse l) = get >>= \s -> when (takeStack s == Number 0) $ executeInstr (Jump l)
    executeInstr (JumpIfTrue l)  = get >>= \s -> when (takeStack s /= Number 0) $ executeInstr (Jump l)
    executeInstr SReturn         = return ()
    executeInstr (SFunCall f n)  = ask >>= \(instrs, lbls) ->
        get >>= \SEnv{..} ->
           liftIO (execStateT  (runReaderT execute (instrs, lbls)) (SEnv emptyVars (take n stack) (findLabel f lbls))) >>=
           \newEnv -> modify (popNstack n) >> modify (pushStack $ takeStack newEnv)
    executeInstr (SRumludeCall f) = executeRumlude f