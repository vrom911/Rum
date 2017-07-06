{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Rum.Compiler.CodeGen where

import           Data.Char           (ord)
import           Control.Monad.State (MonadState, State, execState, gets, modify, void)
import           Data.Map            (Map)
import qualified Data.Map as Map     (empty, insert, lookup, toList)
import           Data.Maybe          (fromMaybe)
import           Data.List           (map, sortBy)
import           Data.Function       (on)
import           Data.Text           (Text)
import qualified Data.Text as T
import           GHC.Word            (Word32)

import qualified LLVM.AST.Global as G  (Global(..), functionDefaults, globalVariableDefaults)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as Ty   (Type(..), i8, i32, ptr, void)
import           LLVM.AST              ( BasicBlock(..), Definition(..)
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
import qualified LLVM.AST.Linkage as L

import qualified Compiler.Rum.Internal.AST as Rum

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
defineFun retType funName argTypes body = addDefn $
  GlobalDefinition $ G.functionDefaults {
    G.name        = Name (T.unpack funName)
  , G.parameters  = ([Parameter parType nm [] | (parType, nm) <- argTypes], False)
  , G.returnType  = retType
  , G.basicBlocks = body
  }

defineIOStrVariable :: String -> String -> LLVM ()
defineIOStrVariable varName formatString = addDefn $
    GlobalDefinition $ G.globalVariableDefaults {
      G.name        = Name varName
    , G.type'       = Ty.ArrayType (fromIntegral $ length formatString) Ty.i8
    , G.isConstant  = True
    , G.initializer = Just $ C.Array Ty.i8 $ map (C.Int 8 . fromIntegral . ord) formatString
    }

declareExtFun :: AST.Type -> Text -> [(AST.Type, Name)] -> Bool -> LLVM ()
declareExtFun retType funName argTypes isVararg = addDefn $
  GlobalDefinition $ G.functionDefaults {
    G.name        = Name (T.unpack funName)
  , G.linkage     = L.External
  , G.parameters  = ([Parameter parType nm [] | (parType, nm) <- argTypes], isVararg)
  , G.returnType  = retType
  , G.basicBlocks = []
  }

-----------------------------
------- Codegen State -------
-----------------------------
type SymbolTable = [(String, Operand)]

-- toplevel module code generation
data CodegenState
  = CodegenState { currentBlock :: Name                     -- Name of the active block to append to
                 , blocks       :: Map Name BlockState      -- Blocks for function
                 , symTable     :: SymbolTable              -- Function scope symbol table
                 , blockCount   :: Int                      -- Count of basic blocks
                 , count        :: Word                     -- Count of unnamed instructions
                 , names        :: Names                    -- Name Supply
                 , varTypes     :: Map String Ty.Type       -- Var name and Type pairs
                 , funRetTypes  :: Map String Ty.Type       -- Fun name and return Type pairs
                 } deriving Show
-- basic blocks inside of function definitions
data BlockState
    = BlockState { idx   :: Int                            -- Block index
                 , stack :: [Named Instruction]            -- Stack of instructions
                 , term  :: Maybe (Named Terminator)       -- Block terminator
                 } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

---------------------------
---------- Types ----------
---------------------------
iType :: AST.Type
iType = Ty.i32

pointer :: AST.Type
pointer = Ty.ptr Ty.i8

arrType :: AST.Type
arrType = Ty.StructureType False [Ty.ptr Ty.i32, Ty.i32]

iBits :: Word32
iBits = 32

fromDataToType :: Rum.DataType -> AST.Type
fromDataToType Rum.UnT = Ty.void
fromDataToType Rum.InT = Ty.i32
fromDataToType Rum.ChT = Ty.i8
fromDataToType Rum.StT = Ty.ptr Ty.i8
fromDataToType (Rum.ArT t) = Ty.StructureType False [Ty.ptr $ fromDataToType t, Ty.i32]

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

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (makeTerm t)
  where
    makeTerm = fromMaybe (error $ "Block has no terminator: " ++ show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty Map.empty Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
    i <- gets count
    let iNew = succ i
    modify $ \s -> s { count = iNew }
    return iNew

tyInstr :: Ty.Type -> Instruction -> Codegen Operand
tyInstr t ins = do
    nm <- fresh
    let ref = UnName nm
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i } )
    return $ local t ref

instr :: Instruction -> Codegen Operand
instr = tyInstr iType

--namedInstr :: String -> Instruction -> Codegen Operand
--namedInstr name instruction = do
--    identfiersNames <- gets names
--    let (newName, newNameMap) = uniqueName name identfiersNames
--    modify $ \codegenState -> codegenState { names = newNameMap }
--    addInstr (Name newName) instruction
--
--addInstr :: Name -> Instruction -> Codegen Operand
--addInstr name instruction = do
--    curBlock  <- current
--    let curStack = stack curBlock
--    modifyBlock (curBlock { stack = curStack ++ [name := instruction] })
--    return $ LocalReference iType name

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  curBlock <- current
  case term curBlock of
      Just oldTrm -> return oldTrm
      Nothing            -> do
          modifyBlock (curBlock { term = Just trm })
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
  return $ Name qname

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
assign v x = gets symTable >>= \symbs -> gets varTypes >>= \varTps ->
    modify (\s -> s { symTable = (v, x) : symbs
                    , varTypes = Map.insert v (typeOfOperand x) varTps})

getVar :: String -> Codegen Operand
getVar var = gets symTable >>= \syms ->
    pure $ fromMaybe (error $ "Local variable not in scope: " ++ show var) (lookup var syms)

----------------------------
-------- References --------
----------------------------
local :: Ty.Type -> Name -> Operand
local = LocalReference

global ::  Ty.Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Ty.Type -> Name -> Operand
externf ty = ConstantOperand . global ty

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
iDiv a b = instr $ SDiv False a b []

iMod :: Operand -> Operand -> Codegen Operand
iMod a b = instr $ SRem a b []

--- logic operations ---
lAnd :: Operand -> Operand -> Codegen Operand
lAnd a b = instr $ And a b []

lOr :: Operand -> Operand -> Codegen Operand
lOr a b = instr $ Or a b []

---  compare operations ---

iCmp :: I.IntegerPredicate -> Operand -> Operand -> Codegen Operand
iCmp cond a b = do
    a' <- instr $ AST.ZExt a iType []
    b' <- instr $ AST.ZExt b iType []
    temp <- instr $ ICmp cond a' b' []
    instr $ AST.ZExt temp iType []

bNeq :: Operand -> Operand -> Codegen Operand
bNeq a b = instr $ AST.ICmp I.NE a b []

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

iNum :: Integer -> Operand
iNum n = cons $ C.Int iBits n

iZero :: Operand
iZero  = iNum 0

isTrue :: Operand -> Codegen Operand
isTrue = iCmp I.NE iZero

isFalse :: Operand -> Codegen Operand
isFalse = iCmp I.EQ iZero

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = tyInstr (typeOfOperand fn) $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen Operand
alloca ty = tyInstr ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = tyInstr (typeOfOperand val) $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = tyInstr (typeOfOperand ptr) $ Load False ptr Nothing 0 []

getElementPtr :: Operand -> Codegen Operand
getElementPtr o = tyInstr (typeOfOperand o) $ GetElementPtr True o [iZero, iZero] []

getElementPtrType :: Operand -> Ty.Type ->Codegen Operand
getElementPtrType o t = tyInstr t $ GetElementPtr True o [iZero, iZero] []

getElementPtrInd :: Operand -> Integer -> Codegen Operand
getElementPtrInd o n = tyInstr (typeOfOperand o) $ GetElementPtr True o [iZero, iNum n] []

getElementPtrIndType :: Operand -> Ty.Type -> Integer -> Codegen Operand
getElementPtrIndType o t n = tyInstr t $ GetElementPtr True o [iZero, iNum n] []

getElementPtrLen :: Operand -> Codegen Operand
getElementPtrLen o = tyInstr iType $ GetElementPtr True o [iZero, iNum 1] []

--getArrayStruct :: Operand -> [C.Constant] -> Codegen Operand
--getArrayStruct o mems = tyInstr (typeOfOperand o) $ C.Struct Nothing False mems []
------------------------
----- Control Flow -----
------------------------
-- Unconditional jump
br :: Name -> Codegen ()
br val = void $ terminator $ Do $ Br val []

-- Conditional jump
cbr :: Operand -> Name -> Name -> Codegen ()
cbr cond tr fl = do
    boolCond <- bNeq cond iZero
    void $ terminator $ Do $ CondBr boolCond tr fl []

-- return command
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []


typeOfOperand :: Operand -> Ty.Type
typeOfOperand (AST.LocalReference t _) = t
typeOfOperand (AST.ConstantOperand C.Int{..}) = iType
typeOfOperand (AST.ConstantOperand (C.GlobalReference t _ )) = t
typeOfOperand (AST.ConstantOperand C.Array{..}) = AST.ArrayType (fromIntegral $ length memberValues) memberType
typeOfOperand (AST.ConstantOperand C.Struct{..}) = AST.StructureType False $ map (typeOfOperand . cons) memberValues
typeOfOperand _ = iType