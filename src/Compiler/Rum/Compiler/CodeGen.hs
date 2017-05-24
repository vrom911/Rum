{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Rum.Compiler.CodeGen where

import           Control.Monad.State (MonadState, State, execState, gets, modify)
import           Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup, toList)
import           Data.Maybe (fromMaybe)
import           Data.List (map, sortBy)
import           Data.Function (on)
import           Data.Text (Text)
import qualified Data.Text as T

import           LLVM.AST.Global (Global(..), functionDefaults)
import qualified LLVM.AST as AST
import           LLVM.AST.Type (i32, void)
import           LLVM.AST  ( BasicBlock(..), Definition(..)
                           , Instruction(..)
                           , Module(..), Name(..)
                           , Named(..)
                           , Operand(..), Parameter(..), Terminator(..)
                           , defaultModule
                           )
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as I

import Compiler.Rum.Internal.AST as Rum (Type(..))

-----------------------
-------- Setup --------
-----------------------
newtype LLVM a = LLVM {stateLLVM :: State Module a}
  deriving (Functor, Applicative, Monad, MonadState Module )

runLLVM :: Module -> LLVM a -> Module
runLLVM modul l = execState (stateLLVM l) modul

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn def = gets moduleDefinitions >>= \defs ->
    modify (\s -> s { moduleDefinitions = defs ++ [def] })

defineFun ::  AST.Type -> Text -> [(AST.Type, Name)] -> [BasicBlock] -> LLVM ()
defineFun ret funName argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name (T.unpack funName)
  , parameters  = ([Parameter typ nm [] | (typ, nm) <- argtys], False)
  , returnType  = ret
  , basicBlocks = body
  }

-----------------------------
------- Codegen State -------
-----------------------------
type SymbolTable = [(String, Operand)]

-- toplevel module code generation
data CodegenState
  = CodegenState { currentBlock :: Name                     -- Name of the active block to append to
                 , blocks       :: Map Name BlockState  -- Blocks for function
                 , symtab       :: SymbolTable              -- Function scope symbol table
                 , blockCount   :: Int                      -- Count of basic blocks
                 , count        :: Word                     -- Count of unnamed instructions
                 , names        :: Names                    -- Name Supply
                 } deriving Show
-- basic blocks inside of function definitions
data BlockState
    = BlockState { idx   :: Int                            -- Block index
                 , stack :: [Named Instruction]            -- Stack of instructions
                 , term  :: Maybe (Named Terminator)       -- Block terminator
                 } deriving Show

---------------------------
---------- Types ----------
---------------------------
tempType :: AST.Type
tempType = i32

getType :: Rum.Type -> AST.Type
getType (Rum.Number _) = i32
getType Rum.Unit = void
--getType (Rum.Str _)    =

-------------------------
--------- Names ---------
-------------------------
type Names = Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

------------------------------
----- Codegen Operations -----
------------------------------
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm = fromMaybe (error $ "Block has no terminator: " ++ show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  let iNew = succ i
  modify $ \s -> s { count = iNew }
  return iNew

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------
--------- Block Stack ---------
-------------------------------
entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
  let (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = succ ix
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen ()
setBlock bname = modify (\s -> s { currentBlock = bname })

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = getBlock >>= \active ->
  modify (\s -> s { blocks = Map.insert active new (blocks s) })

current :: Codegen BlockState
current = getBlock >>= \c -> gets blocks >>= \blks ->
    pure $ fromMaybe (error $ "No such block: " ++ show c) (Map.lookup c blks)

----------------------------
------- Symbol Table -------
----------------------------
assign :: String -> Operand -> Codegen ()
assign v x = gets symtab >>= \lcls ->
    modify (\s -> s {symtab = (v, x) : lcls})

getVar :: String -> Codegen Operand
getVar var = gets symtab >>= \syms ->
    pure $ fromMaybe (error $ "Local variable not in scope: " ++ show var) (lookup var syms)

----------------------------
-------- References --------
----------------------------
local ::  Name -> Operand
local = LocalReference tempType

global ::  Name -> C.Constant
global = C.GlobalReference tempType

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference tempType

----------------------------------
---- Arithmetic and Constants ----
----------------------------------
iAdd :: Operand -> Operand -> Codegen Operand
iAdd a b = instr $ Add False False a b []

iSub :: Operand -> Operand -> Codegen Operand
iSub a b = instr $ Sub False False a b []

iMul :: Operand -> Operand -> Codegen Operand
iMul a b = instr $ Mul False False a b []

iDiv :: Operand -> Operand -> Codegen Operand
iDiv a b = instr $ UDiv False a b []

iMod :: Operand -> Operand -> Codegen Operand
iMod a b = instr $ URem a b []

--- logic operations ---
lAnd :: Operand -> Operand -> Codegen Operand
lAnd a b = instr $ And a b []

lOr :: Operand -> Operand -> Codegen Operand
lOr a b = instr $ Or a b []

---  compare operations ---

iCmp :: I.IntegerPredicate -> Operand -> Operand -> Codegen Operand
iCmp cond a b = instr $ ICmp cond a b []

iEq :: Operand -> Operand -> Codegen Operand
iEq = iCmp I.EQ

iNeq :: Operand -> Operand -> Codegen Operand
iNeq = iCmp I.NE

iNotGt :: Operand -> Operand -> Codegen Operand
iNotGt = iCmp I.SLE

iNotLt :: Operand -> Operand -> Codegen Operand
iNotLt = iCmp I.SGE

iLt :: Operand -> Operand -> Codegen Operand
iLt = iCmp I.SLT

iGt :: Operand -> Operand -> Codegen Operand
iGt = iCmp I.SGT

--------------------------------------
cons :: C.Constant -> Operand
cons = ConstantOperand

iZero :: Operand
iZero  = cons $ C.Int 32 0

isTrue :: Operand -> Codegen Operand
isTrue = iCmp I.NE iZero

isFalse :: Operand -> Codegen Operand
isFalse = iCmp I.EQ iZero

uitofp :: AST.Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

------------------
-- Control Flow --
------------------
-- Unconditional jump
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []
-- Conditional jump
cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- return command
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []