module Rum.Internal.Util
       ( evalRunInterpret
       , runIOInterpret

       , updateVars
       , updateRefVars
       , setRefArrsCell
       , getArrsCell
       , setArrsCell

       , updateFuns
       , updateBool

       , findVar
       , findRefVar
       , findFun
       , fromRefTypeToIO

       , binOp
       , logicOp
       , intCompare
       , isFalse
       , isTrue
       , isUp
       ) where

import Prelude hiding ((<&>))
import Relude.Unsafe ((!!))

import Data.Char (isUpper)

import Rum.Internal.AST (BinOp (..), CompOp (..), Environment (..), Interpret (..), InterpretT,
                         LogicOp (..), RefType (..), RumType (..), Variable (..))

import qualified Data.HashMap.Strict as HM (adjust, insert, lookup)
import qualified Data.Text as T


-------------------------
--- Environment Stuff ---
-------------------------

evalRunInterpret :: Interpret a -> Environment -> MaybeT IO a
evalRunInterpret = evalStateT . runInterpret

runIOInterpret :: Interpret a -> Environment -> IO ()
runIOInterpret action env = () <$ runMaybeT (evalRunInterpret action env)

updateVars :: Variable -> RumType -> Environment ->  Environment
updateVars v val env@Env{..} = env {varEnv = HM.insert v val varEnv}

updateRefVars :: Variable -> IORef RefType -> Environment -> Environment
updateRefVars v val env@Env{..} = env {refVarEnv = HM.insert v val refVarEnv}

setRefArrsCell :: [RumType] -> RumType -> IORef RefType -> IO ()
setRefArrsCell [Number i] x rt = do
    Val (Arr ar) <- readIORef rt
    let (beg, _:rest) = splitAt i ar
    let result        = beg ++ (x:rest)
    writeIORef rt (Val $ Arr result)
setRefArrsCell (Number i:is) x rt = do
    ArrayRef ar <- readIORef rt
    setRefArrsCell is x (ar !! i)
setRefArrsCell ixs _ _ = error $ "called with wrong indices" <> show ixs

updateArrs :: Variable -> [RumType] -> RumType -> Environment -> Environment
updateArrs v inds val env@Env{..} =
    env {varEnv = HM.adjust (\arr -> Arr (setArrsCell inds val arr)) v varEnv}

getArrsCell :: RumType -> [RumType] -> RumType
getArrsCell = foldl' (\(Arr a) (Number i) -> a !! i)

setArrsCell :: [RumType] -> RumType -> RumType -> [RumType]
setArrsCell [Number i] x (Arr ar) =  let (beg, _:rest) = splitAt i ar in beg ++ (x:rest)
setArrsCell (Number i:is) x (Arr ar) = let (beg, cur:rest) = splitAt i ar in beg ++ (Arr (setArrsCell is x cur):rest)
setArrsCell ixs _ _ = error $ "called with wrong indices" <> show ixs

updateFuns
    :: Variable
    -> [Variable]
    -> ([RumType] -> InterpretT)
    -> Environment
    -> Environment
updateFuns name vars prog env@Env{..} = env {funEnv = HM.insert name (vars, prog) funEnv}

updateBool :: Bool -> Environment -> Environment
updateBool b env = env {isReturn = b}

findVar :: Variable -> Environment -> Maybe RumType
findVar x Env{..} = HM.lookup x varEnv

findRefVar :: Variable -> Environment -> Maybe (IORef RefType)
findRefVar x Env{..} = HM.lookup x refVarEnv

findFun :: Variable -> Environment -> Maybe ([Variable], [RumType] -> InterpretT)
findFun x Env{..} = HM.lookup x funEnv

fromRefTypeToIO :: RefType -> IO RumType
fromRefTypeToIO (Val v)      = pure v
fromRefTypeToIO (ArrayRef a) = Arr <$> mapM (readIORef >=> fromRefTypeToIO) a
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

intCompare :: CompOp -> RumType -> RumType -> RumType
intCompare c x y = Number $ bool 0 1 $ compOp c x y

isFalse :: RumType -> Bool
isFalse s = Number 0 == s

isTrue :: RumType -> Bool
isTrue = not . isFalse

isUp :: Variable -> Bool
isUp var = isUpper $ T.head $varName var
