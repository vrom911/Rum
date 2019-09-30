-- Refactoring
-- TODO: перейти на lens

module Rum.Internal.AST where

import Control.Applicative (Alternative)
import Control.Monad.Fail (MonadFail)
import Control.Monad.State (MonadIO, MonadState, StateT)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Hashable (Hashable)
import Data.IORef
import Data.String (IsString, fromString)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HM (HashMap)
import qualified Data.Text as T (pack)


data Type
    = Number Int
    | Ch Char
    | Str Text
    | Arr [Type]
    | Unit
    deriving stock (Show, Eq, Ord)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    deriving stock (Show, Eq, Ord)

data CompOp
    = Eq
    | NotEq
    | Lt
    | Gt
    | NotGt
    | NotLt
    deriving stock (Show, Eq, Ord)

data LogicOp
    = And
    | Or
    | Xor
    deriving stock (Show, Eq, Ord)

newtype Variable = Variable
    { varName :: Text
    } deriving stock (Show, Eq, Ord, Generic)

instance Hashable Variable

instance IsString Variable where
    fromString = Variable . T.pack

data ArrCell = ArrCell
    { arr   :: Variable
    , index :: [Expression]
    } deriving stock (Show)

data FunCall = FunCall
    { fName :: Variable
    , args  :: [Expression]
    } deriving stock (Show)

data Expression
    = Const Type
    | Var   Variable
    | ArrC  ArrCell
    | ArrLit [Expression] -- for [ a, b, c ]
    | Neg   Expression
    | BinOper   {bop :: BinOp,   l, r :: Expression}
    | CompOper  {cop :: CompOp,  l, r :: Expression}
    | LogicOper {lop :: LogicOp, l, r :: Expression}
    | FunCallExp FunCall
    deriving stock (Show)

data Statement
    = AssignmentVar {var :: Variable, value :: Expression}
    | AssignmentArr {arrC :: ArrCell, value :: Expression}
    | FunCallStmt FunCall
    | Skip
    | IfElse      {ifCond    :: Expression, trueAct, falseAct :: Program}
    | RepeatUntil {repCond   :: Expression, act :: Program}
    | WhileDo     {whileCond :: Expression, act :: Program}
    | For         {start  :: Program, expr :: Expression, update, body :: Program}
    | Fun         {funName :: Variable, params :: [Variable], funBody :: Program}
    | Return      {retExp :: Expression}
    deriving stock (Show)

type Program = [Statement]

data RumludeFunName
    = Read
    | Write
    | Strlen
    | Strget
    | Strsub
    | Strdup
    | Strset
    | Strcat
    | Strcmp
    | Strmake
    | Arrlen
    | Arrmake
    deriving stock (Show, Eq, Ord, Generic)

instance Hashable RumludeFunName

-------------------------
--- Environment Stuff ---
-------------------------
newtype Interpret a = Interpret
    { runInterpret :: StateT Environment (MaybeT IO) a
    } deriving anyclass (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState Environment, Alternative)

type InterpretT = Interpret Type

type VarEnv = HM.HashMap Variable Type
type RefVarEnv = HM.HashMap Variable (IORef RefType)
data RefType = Val Type | ArrayRef [IORef RefType]
type FunEnv = HM.HashMap Variable ([Variable], [Type] -> InterpretT)

data Environment = Env
    { varEnv    :: VarEnv
    , refVarEnv :: RefVarEnv
    , funEnv    :: FunEnv
    , isReturn  :: Bool
    }
