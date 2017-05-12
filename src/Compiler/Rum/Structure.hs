{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: создать табличку заранее заданных функций и сделать список заранее заданных функций
--       * read и write уйдут в ту мапу функций
-- TODO: добавить elif
-- TODO: убрать try из funCallP и заменить на varOfFunP
-- TODO: поддержать строки (и все функции с ними)
-- TODO: поддержать массивы (и все функции с ними)

-- Refactoring
-- TODO: заменить StateT Environment (MaybeT IO) Type на newtype Interpret a = Interpret { runInterpet :: StateT ... }
-- TODO: добавить whenT (который как when, но с Type и return Unit)
-- TODO: toString через библиотеку форматирования fmt (красота кода)
-- TODO: заменить String на Text
-- TODO: послушать лекцию и перейти на lens

module Compiler.Rum.Structure where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable             (Hashable)

data Type = Number Int | Str String | Unit          deriving (Show, Eq, Ord)

data BinOp   = Add | Sub | Mul | Div | Mod | Pow    deriving (Show)

data CompOp  = Eq | NotEq | Lt | Gt | NotGt | NotLt deriving (Show)

data LogicOp = And | Or | Xor deriving (Show)

newtype Variable = Variable {name :: String} deriving (Show, Eq, Ord, Hashable)

data Expression = Const Type
                | Var   Variable
                | Neg   Expression
                | BinOper   {bop :: BinOp,   l, r :: Expression}
                | CompOper  {cop :: CompOp,  l, r :: Expression}
                | LogicOper {lop :: LogicOp, l, r :: Expression}
                | FunCall   {fName :: Variable, args :: [Expression]}
                | ReadLn
                deriving (Show)

data Statement  = Assignment  {var :: Variable, value :: Expression}
                | WriteLn     {arg :: Expression}
                | Skip
                | IfElse      {ifCond    :: Expression, trueAct, falseAct :: Program}
                | RepeatUntil {repCond   :: Expression, act :: Program}
                | WhileDo     {whileCond :: Expression, act :: Program}
                | For         {start  :: Program, expr :: Expression, update, body :: Program}
                | Fun         {funName :: Variable, params :: [Variable], funBody :: Program}
                | Return      {retExp :: Expression}
                deriving (Show)

type Program    = [Statement]
-------------------------
--- Environment Stuff ---
-------------------------
type VarEnv = HM.HashMap Variable Type
type FunEnv = HM.HashMap Variable ([Variable], Program)  -- HashMap Variable ([Variable] -> StateT Env (MaybeT IO) Type)
data Environment = Env {varEnv :: VarEnv, funEnv :: FunEnv, isReturn :: Bool}

updateVars :: Variable -> Type -> Environment ->  Environment
updateVars v val env@Env{..} = env {varEnv = HM.insert v val varEnv}

updateFuns :: Variable -> [Variable] -> Program -> Environment -> Environment
updateFuns name vars prog env@Env{..} = env {funEnv = HM.insert name (vars, prog) funEnv}


updateBool :: Bool -> Environment -> Environment
updateBool b env = env {isReturn = b}

findVar :: Variable -> Environment -> Maybe Type
findVar x Env{..} = HM.lookup x varEnv

findFun :: Variable -> Environment -> Maybe ([Variable], Program)
findFun x Env{..} = HM.lookup x funEnv
-------------------------

binOp :: BinOp -> (Int -> Int -> Int)
binOp Add = (+)
binOp Sub = (-)
binOp Mul = (*)
binOp Div = div'
binOp Mod = mod'
binOp Pow = (^)

-- For expressions tests
div' :: Int -> Int -> Int
div' x y =
    case signum x == negate (signum y) of
        True ->
            if absx < absy
                then 0
                else negate $ div absx absy
            where (absx, absy) = (abs x, abs y)
        False -> div x y

mod' :: Int -> Int -> Int
mod' x y = signum x * mod (abs x) (abs y)

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
logicOp Xor = (-!-)

(<&>) :: Int -> Int -> Int
0 <&> _ = 0
_ <&> 0 = 0
_ <&> _ = 1

(-|-) :: Int -> Int -> Int
0 -|- 0 = 0
_ -|- _ = 1

-- hack till idk the operator meaning
(-!-) :: Int -> Int -> Int
(-!-) = (-|-)
