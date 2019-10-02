-- Refactoring
-- TODO: перейти на lens

module Rum.Internal.AST
       ( RumType (..)
       , BinOp (..)
       , CompOp (..)
       , LogicOp (..)
       , Variable (..)
       , ArrCell (..)
       , FunCall (..)
       , Expression (..)
       , Statement (..)
       , Program
       , RumludeFunName (..)

         -- * Environment
       , Environment (..)
       , Interpret (..)
       , InterpretT
       , RefType (..)
       , FunEnv
       ) where

import qualified Data.Text as T (pack)


data RumType
    = Number Int
    | Ch Char
    | Str Text
    | Arr [RumType]
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
    fromString = Variable . toText

data ArrCell = ArrCell
    { arr   :: !Variable
    , index :: ![Expression]
    } deriving stock (Show)

data FunCall = FunCall
    { fName :: !Variable
    , args  :: ![Expression]
    } deriving stock (Show)

data Expression
    = ConstExp RumType
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
    } deriving newtype ( Functor, Applicative, Monad, MonadIO
                       , MonadFail, MonadState Environment
                       , Alternative
                       )

type InterpretT = Interpret RumType

type VarEnv    = HashMap Variable RumType
type RefVarEnv = HashMap Variable (IORef RefType)
type FunEnv    = HashMap Variable ([Variable], [RumType] -> InterpretT)

data RefType
    = Val RumType
    | ArrayRef [IORef RefType]

data Environment = Env
    { varEnv    :: !VarEnv
    , refVarEnv :: !RefVarEnv
    , funEnv    :: !FunEnv
    , isReturn  :: !Bool
    }
