module Rum.StackMachine.Structure
       ( Instruction (..)
       , LabelId (..)
       , Instructions
       , Labels
       , Stack
       , RefSVarEnv
       , SRefType (..)
       , StackEnvironment (..)
       , InterpretStack
       , InterpretStackType
       , RumludeFunNamesMap
       ) where

import Rum.Internal.AST (RumType, RumludeFunName, Variable, LogicOp, BinOp, CompOp)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


-- | Wrapper around instruction label.
newtype LabelId = LabelId
    { unLabelId :: Text
    } deriving stock (Show, Eq, Ord, Generic)
      deriving anyclass (Hashable)

instance IsString LabelId where
    fromString = LabelId . toText

-- | Represents possible Rum's stack instruction.
data Instruction
    = Nop       -- ^ No operation
    | SBinOp BinOp
    | SLogicOp LogicOp
    | SCompOp CompOp
    | Push RumType   -- ^ Push a value onto the stack
    | Pop       -- ^ Pop the most recent value from the stack
    | Jump LabelId      -- ^ Jump unconditionally to a location
    | JumpIfTrue LabelId
    | JumpIfFalse LabelId
    | Load Variable
    | LoadRef Variable
    | Store Variable
    | Label LabelId
    | SFunCall LabelId Int    -- ^ Call a function
    | SReturn       -- ^ Return from a function
    | SRumludeCall RumludeFunName  -- ^ Call function from standard library
    | PushNArr Int
    | LoadArr Variable Int
    | StoreArr Variable Int
    deriving (Show)

type Instructions = State Int [Instruction]
type Labels = HM.HashMap LabelId Int
type Stack = [RumType]
type RefSVarEnv = HM.HashMap Variable SRefType

data SRefType
    = Val RumType
    | RefVal (IORef RumType)

data StackEnvironment = SEnv
    { vars  :: !RefSVarEnv
    , stack :: !Stack
    , pos   :: !Int
    }

type InterpretStack = ReaderT ([Instruction], Labels) (StateT StackEnvironment IO) ()
type InterpretStackType = ReaderT ([Instruction], Labels) (StateT StackEnvironment IO) RumType
type RumludeFunNamesMap = HM.HashMap Text RumludeFunName
