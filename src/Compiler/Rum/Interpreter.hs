{-# LANGUAGE RecordWildCards #-}

module Compiler.Rum.Interpreter where

import           Control.Monad             (void)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HM (insert)

import           Compiler.Rum.Structure


interpret :: Program -> Reader Environment ()
interpret = foldr ((>>) . interpretSt) (return ())


interpretSt :: Statement -> Reader Environment ()
interpretSt Assignment{..} = eval value >>= \x -> local (HM.insert var (evalInt x)) $ return ()
interpretSt Skip = return ()
interpretSt IfElse{..} = eval ifCond >>= \x ->
                                        if x == Just 0 then interpret falseAct else interpret trueAct
interpretSt st@WhileDo{..} = do
    c <- eval whileCond
    when (c /= Just 0) $ do
        interpret act
        interpretSt st
interpretSt st@RepeatUntil{..} = do
    interpret act
    c <- eval repCond
    when (c /= Just 0) $ interpretSt st
interpretSt For{..} = do
    interpret start
    forDo
  where
    forDo = do
        c <- eval expr
        when (c /= Just 0) $ do
            interpret body
            interpret update
            forDo
interpretSt WriteLn{..} = void $ eval arg

evalInt :: Maybe Int -> Int
evalInt (Just c) = c
evalInt Nothing = error "Something wrong"