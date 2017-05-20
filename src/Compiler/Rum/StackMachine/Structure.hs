{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Compiler.Rum.StackMachine.Structure where

import           Control.Monad.State
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import           Data.Hashable             (Hashable)
import           Data.String               (IsString, fromString)
import           Data.Text                 (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Compiler.Rum.Internal.AST

newtype LabelId = LabelId Text deriving (Show, Eq, Ord, Generic)
instance Hashable LabelId
instance IsString LabelId where
    fromString = LabelId . T.pack

data RumludeFunName = SRead  | SWrite
                    | Strlen | Strget | Strsub | Strdup | Strset | Strcat | Strcmp | Strmake
                    | Arrlen | Arrmake
                    deriving (Show, Eq, Ord, Generic)
instance Hashable RumludeFunName

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
                 | SFunCall LabelId Int    -- Call a function
                 | SReturn       -- Return from a function
                 | SRumludeCall RumludeFunName  -- Call function from standard library
               deriving (Show)

type Instructions = State Int [Instruction]
type Labels = HM.HashMap LabelId Int
type Stack = [Type]
data StackEnvironment = SEnv { vars :: VarEnv
                             , stack :: Stack
                             , pos :: Int
                             }
type InterpretStack = ReaderT ([Instruction], Labels) (StateT StackEnvironment IO) ()
type InterpretStackType = ReaderT ([Instruction], Labels) (StateT StackEnvironment IO) Type
type RumludeFunNamesMap = HM.HashMap Text RumludeFunName