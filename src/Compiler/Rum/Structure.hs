{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Rum.Structure where


import           Data.Bool (bool)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable              (Hashable)
import           Control.Monad.Reader
import           Control.Applicative        (liftA2)
import           Text.Read                  (readMaybe)
import           System.IO.Unsafe

type Program = [Statement]

data BinOp   = Add | Sub | Mul | Div | Mod | Pow    deriving (Show)

data CompOp  = Eq | NotEq | Lt | Gt | NotGt | NotLt deriving (Show)

data LogicOp = And | Or deriving (Show)

newtype Variable = Variable {name :: String} deriving (Show, Eq, Ord, Hashable)

data Expression = Const Int
                | Var   Variable
                | Neg   Expression
                | BinOper   {bop :: BinOp,   l, r :: Expression}
                | CompOper  {cop :: CompOp,  l, r :: Expression}
                | LogicOper {lop :: LogicOp, l, r :: Expression}
                | ReadLn
                deriving (Show)

data Statement = Assignment  {var :: Variable, value :: Expression}
               | WriteLn     {arg :: Expression}
               | Skip
               | IfElse      {ifCond    :: Expression, trueAct, falseAct :: Program}
               | RepeatUntil {repCond   :: Expression, act :: Program}
               | WhileDo     {whileCond :: Expression, act :: Program}
               | For         {start :: Program, expr :: Expression, update, body :: Program}
               deriving (Show)

type Environment = HM.HashMap Variable Int

binOp :: (Integral a) => BinOp -> (a -> a -> a)
binOp Add = (+)
binOp Sub = (-)
binOp Mul = (*)
binOp Div = div
binOp Mod = mod
binOp Pow = (^)

compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Eq    = (==)
compOp NotEq = (/=)
compOp Lt    = (<)
compOp Gt    = (>)
compOp NotGt = (<=)
compOp NotLt = (>=)

logicOp :: LogicOp -> Int -> Int -> Int
logicOp And = (<&>)
logicOp Or  = (-|-)

(<&>) :: Int -> Int -> Int
0 <&> _ = 0
_ <&> 0 = 0
_ <&> _ = 1

(-|-) :: Int -> Int -> Int
0 -|- 0 = 0
_ -|- _ = 1

eval :: Expression -> Reader Environment (Maybe Int)
eval (Const c)     = pure (Just c)
eval (Var v)       = asks (HM.lookup v)
eval (Neg e)       = (negate <$>) <$> eval e
eval BinOper{..}   = liftA2 (liftA2 (binOp bop))   (eval l) (eval r)
eval LogicOper{..} = liftA2 (liftA2 (logicOp lop)) (eval l) (eval r)
eval CompOper{..}  = liftA2 (liftA2 intCompare) (eval l) (eval r)
  where
    intCompare :: Int -> Int -> Int
    intCompare x y = bool 0 1 $ compOp cop x y
eval ReadLn        = return $ unsafePerformIO (do
                        input <- getLine
                        return $ readMaybe input)
