{- | Functions to run Rum's language interpreter
-}

module Rum.Interpreter.Rummer
       ( rumInterpreter
       )where

import Control.Applicative (empty, liftA2)
import Control.Monad.State (MonadState, get, gets, lift, liftIO, modify)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Rum.Internal.AST
import Rum.Internal.Rumlude (runRumlude)
import Rum.Internal.Util
import Rum.Interpreter.Rumlude (preludeLibrary)

import qualified Data.HashMap.Strict as HM (fromList, lookup)


-------------------
----- Rummer ------
-------------------

rumInterpreter :: [Statement] -> IO ()
rumInterpreter p = runIOInterpret (interpret p) (Env mempty mempty preludeLibrary False)

interpret :: Program -> InterpretT
interpret [] = return Unit
interpret (st:stmts) = do
    stRes <- interpretSt st
    x <- gets isReturn
    if x then return stRes else interpret stmts

interpretSt :: Statement -> InterpretT
interpretSt AssignmentVar{..}  = eval value >>= \x ->
    if isUp var
    then do
        refs <- gets refVarEnv
        case HM.lookup var refs of
            Just r  -> liftIO $ writeIORef r (Val x) >> pure Unit
            Nothing -> liftIO (newIORef$ Val x) >>= \rx -> modifyT (updateRefVars var rx)
    else modifyT (updateVars var x)
interpretSt AssignmentArr{..}  = do
    x <- eval value
    inds <- mapM eval (index arrC)
    refs <- gets refVarEnv
    let Just r = HM.lookup (arr arrC) refs
    liftIO (setRefArrsCell inds x r) >> pure Unit
interpretSt Skip               = return Unit
interpretSt IfElse{..}         = eval ifCond >>= \x ->
    if isFalse x then interpret falseAct else interpret trueAct
interpretSt st@WhileDo{..}     = do
    c <- eval whileCond
    whenT (isTrue c) $ interpret act >> interpretSt st
interpretSt st@RepeatUntil{..} = do
    () <$ interpret act
    c <- eval repCond
    whenT (isFalse c) $ interpretSt st
interpretSt For{..} = interpret start *> forDo
  where
    forDo = do
        c <- eval expr
        whenT (isTrue c) $ interpret body >> interpret update >> forDo
interpretSt Fun{..} = modifyT (updateFuns funName params $ \_ -> interpret funBody)
interpretSt Return{..} = modify (updateBool True) >> eval retExp
interpretSt (FunCallStmt f) = evalFunCall f

{-
 StateT s (MaybeT IO) a = s -> IO (Maybe (a, s))
 MaybeT IO a = IO (Maybe a)
 pure x :: a -> m a = a -> StateT s (MaybeT IO) a = a -> (s -> IO (Maybe (a, s))
 empty :: StateT s (MaybeT IO) a = s -> IO Nothing
-}
eval :: Expression -> InterpretT
eval (Const c)     = pure c
eval (Var v)       =
    if isUp v
    then do
        refVar <- gets (findRefVar v)
        case refVar of
            Just x  -> liftIO $ readIORef x >>= fromRefTypeToIO
            Nothing -> evalVar v
    else  evalVar v
eval (ArrC ArrCell{..}) = do
    inds <- mapM eval index
    var <- gets (findRefVar arr)
    case var of
        Just refvar -> liftIO $ readIORef refvar >>= fromRefTypeToIO >>= \x ->
                return $ getArrsCell x inds
        Nothing -> gets (findVar arr) >>= \v -> case v of
            Just v  -> return $ getArrsCell v inds
            Nothing -> empty
eval (ArrLit exps) = Arr <$> mapM eval exps
eval (Neg e)       = do
    Number x <- eval e
    pure $ Number (negate x)
eval BinOper {..} = do
    Number left <- eval l
    Number right <- eval r
    pure $ Number (binOp bop left right)
eval LogicOper{..} = do
    Number left <- eval l
    Number right <- eval r
    pure $ Number (logicOp lop left right)
eval CompOper{..}  = liftA2 (intCompare cop) (eval l) (eval r)
eval (FunCallExp f) = evalFunCall f


evalFunCall :: FunCall -> InterpretT
evalFunCall FunCall{fName = Variable "strset", args = [Var vS, i, c]} = do
    test <- gets (findRefVar vS)
    valStr <- case test of
        Just v -> pure v
        Nothing -> do
            Just v <- gets (findVar vS)
            liftIO $ newIORef (Val v)
    Val s <- liftIO $ readIORef valStr
    evI <- eval i
    evC <- eval c
    interpretSt $ AssignmentVar vS (Const $ runRumlude Strset [s, evI, evC])
evalFunCall FunCall{..} = do
    env <- get
    let funs = funEnv env
--    let globals = varEnv env
    evalArgs          <- mapM eval args
    Just (names, fun) <- gets (findFun fName)
    let (locs, refs) = temp names args evalArgs env
    let locals = HM.fromList locs
    let ref = HM.fromList refs
    Interpret $ lift $ evalRunInterpret (fun evalArgs) (Env locals {-`HM.union` globals-} ref funs False)

temp :: [Variable] -> [Expression] -> [Type] -> Environment -> ([(Variable, Type)], [(Variable, IORef RefType)])
temp [] [] [] _ = (mempty, mempty)
temp (v:vs) (e:es) (t:ts) env = case e of
    Var var -> let refVar = findRefVar var env in
        case refVar of
            Just x  -> ((v, t):l, (v, x): r)
            Nothing -> ((v, t):l, r)
    _ -> ((v, t):l, r)
  where
    (l, r) = temp vs es ts env


evalVar :: Variable -> InterpretT
evalVar v = do
    var <- gets (findVar v)
    case var of
        Just x  -> pure x
        Nothing -> empty
--------------
---- Util ----
--------------

whenT :: (Applicative f) => Bool -> f Type -> f Type
whenT cond s  = if cond then s else pure Unit

modifyT :: MonadState s m => (s -> s) -> m Type
modifyT f = modify f >> pure Unit
