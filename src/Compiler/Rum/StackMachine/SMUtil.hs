module Compiler.Rum.StackMachine.SMUtil where

import Data.String (fromString)
import           Data.Text      (Text)
import qualified Data.Text as T
import           Safe           (headMay)

import Compiler.Rum.Internal.AST
import Compiler.Rum.StackMachine.Structure

-----------------
-- Environment --
-----------------

pushStack :: Type -> StackEnvironment -> StackEnvironment
pushStack el sEnv@SEnv{..} = sEnv{stack = el : stack}

popStack :: StackEnvironment -> StackEnvironment
popStack sEnv@SEnv{..} = case stack of
    [] -> error "Can not pop from empty stack"
    _  -> sEnv{stack = tail stack}

takeStack :: StackEnvironment -> Maybe Type
takeStack SEnv{..} = headMay stack

---------------
---- Label ----
---------------

buildLabel :: Int -> LabelId
buildLabel n = fromString ("label" ++ show n)
