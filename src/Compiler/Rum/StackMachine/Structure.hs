module Compiler.Rum.StackMachine.Structure where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import           Data.String         (IsString, fromString)
import           Data.Text           (Text)
import qualified Data.Text as T

import Compiler.Rum.Internal.AST

newtype LabelId = LabelId Text deriving (Show)
instance IsString LabelId where
    fromString = LabelId . T.pack

data RumludeFunName = SRead | SWrite deriving (Show)
data Instruction = Nop       -- No operation
                | SBinOp BinOp
                | SLogicOp LogicOp
                | SCompOp CompOp
                | Push Type   -- Push a value onto the stack
                | Pop       -- Pop the most recent value from the stack
                | Jump LabelId      -- Jump unconditionally to a location
                | JumpIfTrue LabelId
                | JumpIfFalse LabelId
                | Load Variable
                | Store Variable
                | Label LabelId
                | SFunCall LabelId     -- Call a function
                | SReturn       -- Return from a function
                | SRumludeCall RumludeFunName  -- Call function from standard library
               deriving (Show)

type Instructions = State Int [Instruction]

type Stack = [Type]
type SFunEnv = HM.HashMap Variable ([Variable], Program)
data SEnvironment = Env {varEnv :: VarEnv, funEnv :: SFunEnv, isReturn :: Bool}
data StackEnvironment = SEnv { env :: SEnvironment
                             , stack :: Stack
                             , pos :: Int
                             }

rumludeFunNames :: HM.HashMap Text RumludeFunName
rumludeFunNames = HM.fromList [("read", SRead), ("write", SWrite)]