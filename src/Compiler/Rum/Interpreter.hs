{-# LANGUAGE RecordWildCards #-}

module Compiler.Rum.Interpreter where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM (insert)
import Data.Maybe (fromMaybe)

import           Compiler.Rum.Structure


interpret :: Program -> StateT Environment IO ()
interpret = foldr ((>>) . interpretSt) (return ())


interpretSt :: Statement -> StateT Environment IO ()
interpretSt Assignment{..}     = eval value >>= \x ->
    modify (HM.insert var (fromMaybe (error "Expr error") x))
interpretSt Skip               = return ()
interpretSt IfElse{..}         = eval ifCond >>= \x ->
    if x == Just 0 then interpret falseAct else interpret trueAct
interpretSt st@WhileDo{..}     = do
    c <- eval whileCond
    when (c /= Just 0) $ do
        interpret act
        interpretSt st
interpretSt st@RepeatUntil{..} = do
    interpret act
    c <- eval repCond
    when (c == Just 0) $ interpretSt st
interpretSt For{..}            = do
    interpret start
    forDo
  where
    forDo = do
        c <- eval expr
        when (c /= Just 0) $ do
            interpret body
            interpret update
            forDo
interpretSt WriteLn {..} = do
    res <- eval arg
--    lift $ putStr "> > " -- for compiler-test/expressions
    lift $ putStr "> > > > "-- for compiler-test/deep-expressions
    liftIO $ print $ fromMaybe (error "WriteLn error") res  -- res ?: error "writeln error"