{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Refactoring
-- TODO: перейти на lens

module Compiler.Rum.Internal.AST where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable             (Hashable)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe


data Type = Number Int | Ch Char | Str Text | Arr [Type] | Unit deriving (Show, Eq, Ord)

data BinOp   = Add | Sub | Mul | Div | Mod | Pow    deriving (Show)

data CompOp  = Eq | NotEq | Lt | Gt | NotGt | NotLt deriving (Show)

data LogicOp = And | Or | Xor deriving (Show)

newtype Variable = Variable {name :: Text} deriving (Show, Eq, Ord, Hashable)

instance IsString Variable where
    fromString = Variable . T.pack

data ArrCell  = ArrCell {arr :: Variable, index :: [Expression]} deriving (Show)
data FunCall = FunCall {fName :: Variable, args :: [Expression]} deriving (Show)
data Expression = Const Type
                | Var   Variable
                | ArrC  ArrCell
                | ArrLit [Expression]
                | Neg   Expression
                | BinOper   {bop :: BinOp,   l, r :: Expression}
                | CompOper  {cop :: CompOp,  l, r :: Expression}
                | LogicOper {lop :: LogicOp, l, r :: Expression}
                | FunCallExp FunCall
                deriving (Show)

data Statement  = AssignmentVar {var :: Variable, value :: Expression}
                | AssignmentArr {arrC :: ArrCell, value :: Expression}
                | FunCallStmt FunCall
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
newtype Interpret a = Interpret
    { runInterpret :: StateT Environment (MaybeT IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, Alternative)

evalRunInterpret :: Interpret a -> Environment -> MaybeT IO a
evalRunInterpret = evalStateT . runInterpret

runIOInterpret :: Interpret a -> Environment -> IO ()
runIOInterpret action env = () <$ runMaybeT (evalRunInterpret action env)

type InterpretT = Interpret Type

type VarEnv = HM.HashMap Variable Type
type FunEnv = HM.HashMap Variable ([Variable], [Type] -> InterpretT)
data Environment = Env {varEnv :: VarEnv, funEnv :: FunEnv, isReturn :: Bool}

updateVars :: Variable -> Type -> Environment ->  Environment
updateVars v val env@Env{..} = env {varEnv = HM.insert v val varEnv}

updateArrs :: Variable -> [Type] -> Type -> Environment ->  Environment
updateArrs v inds val env@Env{..} =
    let Just arr = HM.lookup v varEnv in
    env {varEnv = HM.insert v (Arr (setArrsCell inds val arr)) varEnv}

setArrsCell :: [Type] -> Type -> Type -> [Type]
setArrsCell [Number i] x (Arr ar) =  let (beg, _:rest) = splitAt i ar in beg ++ (x:rest)
setArrsCell (Number i:is) x (Arr ar) = let (beg, cur:rest) = splitAt i ar in beg ++ (Arr (setArrsCell is x cur):rest)
setArrsCell ixs _ _ = error $ "called with wrong indices" ++ show ixs

updateFuns :: Variable -> [Variable] -> ([Type] -> InterpretT) -> Environment -> Environment
updateFuns name vars prog env@Env{..} = env {funEnv = HM.insert name (vars, prog) funEnv}

updateBool :: Bool -> Environment -> Environment
updateBool b env = env {isReturn = b}

findVar :: Variable -> Environment -> Maybe Type
findVar x Env{..} = HM.lookup x varEnv

findFun :: Variable -> Environment -> Maybe ([Variable], [Type] -> InterpretT)
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


