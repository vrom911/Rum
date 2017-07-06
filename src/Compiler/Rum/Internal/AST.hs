{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- Refactoring
-- TODO: перейти на lens

module Compiler.Rum.Internal.AST where

import           Control.Applicative       (Alternative)
import           Control.Monad.State       (MonadIO, MonadState, StateT)
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.HashMap.Strict as HM (HashMap)
import           Data.Hashable             (Hashable)
import           Data.IORef
import           Data.String               (IsString, fromString)
import           Data.Text                 (Text)
import qualified Data.Text as T            (pack)
import           GHC.Generics              (Generic)

data DataType = InT | ChT | StT | ArT DataType | UnT deriving (Show)

data Type = Number Int | Ch Char | Str Text | Arr ([Type], Int) | Unit deriving (Show, Eq, Ord)

data BinOp   = Add | Sub | Mul | Div | Mod | Pow    deriving (Show, Eq, Ord)

data CompOp  = Eq | NotEq | Lt | Gt | NotGt | NotLt deriving (Show, Eq, Ord)

data LogicOp = And | Or | Xor deriving (Show, Eq, Ord)

newtype Variable = Variable {varName :: Text} deriving (Show, Eq, Ord, Hashable)

instance IsString Variable where
    fromString = Variable . T.pack

data ArrCell  = ArrCell {arr :: Variable, index :: [Expression]} deriving (Show)
data FunCall  = FunCall {fName :: Variable, args :: [Expression]} deriving (Show)
data Expression = Const Type
                | Var   Variable
                | ArrC  ArrCell       -- for a[i][j]
                | ArrLit [Expression] -- for [ a, b, c ]
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
                | Fun         { funName    :: Variable
                              , params     :: [(Variable, DataType)]
                              , funBody    :: Program
                              , retType    :: DataType
                              }
                | Return      {retExp :: Expression}
                deriving (Show)

type Program    = [Statement]

data RumludeFunName = Read   | Write
                    | Strlen | Strget | Strsub | Strdup | Strset | Strcat | Strcmp | Strmake | WriteStr
                    | Arrlen | Arrmake
                    deriving (Show, Eq, Ord, Generic)
instance Hashable RumludeFunName

-------------------------
--- Environment Stuff ---
-------------------------
newtype Interpret a = Interpret
    { runInterpret :: StateT Environment (MaybeT IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, Alternative)

type InterpretT = Interpret Type

type VarEnv = HM.HashMap Variable Type
type RefVarEnv = HM.HashMap Variable (IORef RefType)
data RefType = Val Type | ArrayRef ([IORef RefType], Int)
type FunEnv = HM.HashMap Variable ([(Variable, DataType)], [Type] -> InterpretT)
data Environment = Env { varEnv    :: VarEnv
                       , refVarEnv :: RefVarEnv
                       , funEnv    :: FunEnv
                       , isReturn  :: Bool
                       }