module Rum.StackMachine.Stacker
       ( rumStacker
       ) where

import Relude.Unsafe ((!!))

import Rum.Internal.Rumlude (rumludeFunArgs, runRumlude, writeRumlude)
import Rum.Internal.Util (binOp, intCompare, isFalse, isUp, logicOp)
import Rum.StackMachine.Structure (Instruction (..), InterpretStack, SRefType (..),
                                   StackEnvironment (..))
import Rum.StackMachine.Translator (translateP)
import Rum.StackMachine.Util (buildLabelsMap, emptyVars, findLabel, findSVar, getSArrayCell,
                              popNstack, popStack, pushStack, succPos, takePopStack, takeStack,
                              updatePos, updateSArrs, updateSVars, updateSmallVars)

import qualified Data.HashMap.Strict as HM (lookup)
import qualified Rum.Internal.AST as AST


rumStacker :: AST.Program -> IO ()
rumStacker p = do
    let instrs = evalState (translateP p) 0
--    putStrLn $ unlines $ map show instrs
    evalStateT (runReaderT stacker (instrs, buildLabelsMap instrs)) (SEnv emptyVars [] 0)


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
    isReturn _       = False

    executeInstr :: Instruction -> InterpretStack
    executeInstr Nop        = pass
    executeInstr (Label _)  = pass
    executeInstr (Push x)   = push x
    executeInstr Pop        = pop
    executeInstr (PushNArr n) = replicateM n takePopStack >>= \arr -> push (AST.Arr $ reverse arr)
    executeInstr (LoadArr ar n) = do
        indexes <- replicateM n takePopStack
        rAr <- gets (findSVar ar)
        res <- liftIO $ getSArrayCell rAr (reverse indexes)
        push res
    executeInstr (StoreArr ar n) = replicateM n takePopStack >>= \indexes ->
        takePopStack >>= \val -> get >>= liftIO . updateSArrs ar (reverse indexes) val
    executeInstr (SBinOp b) =
        gets takeStack >>= \(AST.Number y) ->
            pop >> gets takeStack >>= \(AST.Number x) ->
                pop >> push (AST.Number (binOp b x y))
    executeInstr (SLogicOp l) =
        gets takeStack >>= \(AST.Number y) ->
            pop >> gets takeStack >>= \(AST.Number x) ->
                pop >> push (AST.Number (logicOp l x y))
    executeInstr (SCompOp c)  = gets takeStack >>= \y ->
        pop >> gets takeStack >>= \x ->
            pop >> push (intCompare c x y)
    executeInstr (Load v) = gets (findSVar v) >>= \case
        Val y    -> push y
        RefVal y -> liftIO (readIORef y) >>= push
    executeInstr (LoadRef v) = gets (findSVar v) >>= \case
        RefVal y -> liftIO (readIORef y) >>= push
        Val y    -> push y
    executeInstr (Store v) =
        if not (isUp v)
        then modify (updateSmallVars v) >> pop
        else do
            mx <- gets (viaNonEmpty head . stack)
            whenJust mx $ \x -> do
                vs <- gets vars
                case HM.lookup v vs of
                    Just (RefVal m) -> liftIO $ writeIORef m x
                    Nothing   -> liftIO (newIORef x) >>= \rx -> modify (updateSVars v (RefVal rx))
                pop

    executeInstr (Jump l)        = asks snd >>= \lbls -> modify (updatePos (findLabel l lbls))
    executeInstr (JumpIfFalse l) = gets takeStack >>= \s -> when (isFalse s) $ executeInstr (Jump l)
    executeInstr (JumpIfTrue l)  = gets takeStack >>= \s -> unless (isFalse s) $ executeInstr (Jump l)
    executeInstr SReturn         = pass
    executeInstr (SFunCall f n)  = ask >>= \(instrs, lbls) ->
        get >>= \SEnv{..} ->
           liftIO (execStateT  (runReaderT execute (instrs, lbls)) (SEnv emptyVars (take n stack) (findLabel f lbls))) >>=
           \newEnv -> modify (popNstack n) >> push (takeStack newEnv)
    executeInstr (SRumludeCall f) = executeRumlude f


    executeRumlude :: AST.RumludeFunName -> InterpretStack
    executeRumlude AST.Read = liftIO getLine >>= \input ->
        modify (pushStack $ AST.Number $ fromMaybe (error "Wrong input") (readMaybe $ toString input))
    executeRumlude AST.Write = takePopStack >>= writeRumlude
    executeRumlude f = replicateM (fromMaybe (error "Something gone wrong") (HM.lookup f rumludeFunArgs)) takePopStack >>=
        \args -> push (runRumlude f $ reverse args)

    push :: AST.RumType -> InterpretStack
    push = modify . pushStack

    pop :: InterpretStack
    pop = modify popStack
