{-# LANGUAGE RecordWildCards #-}

module Compiler.Rum.Interpreter where

import           Control.Applicative       (liftA2, empty)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bool                 (bool)
import qualified Data.HashMap.Strict as HM (fromList)
import           Text.Read                 (readMaybe)

import           Compiler.Rum.Structure
import           Compiler.Rum.ToString (typeToStr)


interpret :: Program -> StateT Environment (MaybeT IO) Type
interpret [] = return Unit
interpret (st:stmts) = do
    stRes <- interpretSt st
    x <- gets isReturn
    if x then return stRes else interpret stmts

interpretSt :: Statement -> StateT Environment (MaybeT IO) Type
interpretSt Assignment{..}     = eval value >>= \x -> modify (updateVars var x) >> return Unit
interpretSt Skip               = return Unit
interpretSt IfElse{..}         = eval ifCond >>= \x ->
    if x == Number 0 then interpret falseAct else interpret trueAct
interpretSt st@WhileDo{..}     = do
    c <- eval whileCond
    if c /= Number 0 then interpret act >> interpretSt st
    else return Unit
interpretSt st@RepeatUntil{..} = do
    () <$ interpret act
    c <- eval repCond
    if c == Number 0 then interpretSt st else return Unit
interpretSt For{..} = interpret start *> forDo
  where
    forDo = do
        c <- eval expr
        if c /= Number 0 then do
            () <$ interpret body
            () <$ interpret update
            forDo
        else
            return Unit
interpretSt WriteLn{..} = do
    res <- eval arg
--    lift $ putStr "> > " -- for compiler-test/expressions
--    liftIO $ putStr "> > > > "-- for compiler-test/deep-expressions
    liftIO $ putStrLn $ typeToStr res  -- res ?: error "writeln error"
    return Unit
interpretSt Fun{..} = modify (updateFuns funName params funBody) >> return Unit
interpretSt Return{..} = modify (updateBool True) >> eval retExp

{-
 StateT s (MaybeT IO) a = s -> IO (Maybe (a, s))
 MaybeT IO a = IO (Maybe a)
 pure x :: a -> m a = a -> StateT s (MaybeT IO) a = a -> (s -> IO (Maybe (a, s))
 empty :: StateT s (MaybeT IO) a = s -> IO Nothing
-}
eval :: Expression -> StateT Environment (MaybeT IO) Type  -- Environment -> IO (Maybe (Type, Environment))
eval (Const c)     = pure c
eval (Var v)       = do
    var <- gets (findVar v)
    -- maybe empty pure var
    case var of
        Nothing -> empty
        Just x  -> pure x
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
eval CompOper{..}  = liftA2 intCompare (eval l) (eval r)
  where
    intCompare :: Type -> Type -> Type
    intCompare x y = Number $ bool 0 1 $ compOp cop x y
eval ReadLn        = do
    input <- liftIO getLine
    case readMaybe input of
        Nothing -> empty
        Just x -> pure (Number x)
eval FunCall{..} = do
   env <- get
   let funs = funEnv env
   a <- mapM eval args
   Just (names, stm) <- gets (findFun fName)
--   let Just (names, stm) = pair-- HM.lookup fName funs
   let locals = HM.fromList (zip names a)
   lift $ evalStateT (interpret stm) (Env locals funs False)