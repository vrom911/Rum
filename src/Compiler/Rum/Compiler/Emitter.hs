module Compiler.Rum.Compiler.Emitter where



import           Control.Monad.Except (ExceptT, forM_, runExceptT, (>=>))
import           Data.Map             (Map)
import qualified Data.Map as Map      (fromList, lookup)
import qualified Data.Text as T

import qualified LLVM.AST as AST            (Operand(..), Type(..), Module(..), Name(..), Operand(..))
import qualified LLVM.AST.Constant as C     (Constant(..))
import           LLVM.Context               (withContext)
import           LLVM.Module                (moduleLLVMAssembly, withModuleFromAST)

import           Compiler.Rum.Compiler.CodeGen
import qualified Compiler.Rum.Internal.AST as Rum

codeGenAll :: Rum.Program -> LLVM ()
codeGenAll pr = let (funs, main) = span isFunDeclSt pr in
    codeGenTops funs >> codeGenMain main



toSig :: [Rum.Variable] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (tempType, AST.Name (T.unpack $ Rum.varName x)))

isFunDeclSt :: Rum.Statement -> Bool
isFunDeclSt Rum.Fun{} = True
isFunDeclSt _     = False

codeGenTops = foldr ((>>) . codeGenTop) (return iZero)

codeGenTop :: Rum.Statement -> LLVM ()
codeGenTop Rum.Fun{..} = defineFun tempType (Rum.varName funName) fnargs bls
  where
    fnargs = toSig params
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ params $ \a -> do
        var <- alloca tempType
        store var (local (AST.Name (T.unpack $ Rum.varName a)))
        assign (T.unpack $ Rum.varName a) var
      codeGenFunProg funBody >>= ret

codeGenMain :: Rum.Program -> LLVM ()
codeGenMain pr = defineFun tempType "main" [] bls
  where
    bls = createBlocks $ execCodegen $ do
        entry <- addBlock "main"
        setBlock entry
        codeGenProg pr
        ret iZero

codeGenFunProg :: Rum.Program -> Codegen AST.Operand
codeGenFunProg (Rum.Return{..}:stmts) = cgenExpr retExp
codeGenFunProg (s:stmts) = codeGenStmt s >> codeGenFunProg stmts

codeGenProg :: Rum.Program -> Codegen ()
codeGenProg= foldr ((>>) . codeGenStmt) (return ())

codeGenStmt :: Rum.Statement -> Codegen ()
codeGenStmt Rum.Skip = return ()
codeGenStmt Rum.AssignmentVar{..} = do
    v <- alloca tempType
    cgenedVal <- cgenExpr value
    store v cgenedVal
    assign (T.unpack $ Rum.varName var) v
    return ()

codeGenStmt Rum.IfElse{..} = do
    ifTrueBlock <- addBlock "if.then"
    elseBlock   <- addBlock "if.else"
    ifExitBlock <- addBlock "if.exit"
    -- %entry
    ------------------
    cond <- cgenExpr ifCond
    test <- isTrue cond
    cbr test ifTrueBlock elseBlock -- Branch based on the condition
    -- if.then
    ------------------
    setBlock ifTrueBlock
    trval <- codeGenProg trueAct       -- Generate code for the true branch
    br ifExitBlock              -- Branch to the merge block
    ifthen <- getBlock
    -- if.else
    ------------------
    setBlock elseBlock
    flval <- codeGenProg falseAct       -- Generate code for the false branch
    br ifExitBlock              -- Branch to the merge block
    ifelse <- getBlock
    -- if.exit
    ------------------
    setBlock ifExitBlock
    return ()

codeGenStmt Rum.RepeatUntil{..} = do
    repeatBlock <- addBlock "repeat.loop"
    condBlock   <- addBlock "repeat.cond"
    exitBlock   <- addBlock "repeat.end"

    br repeatBlock
    -- repeat-body
    setBlock repeatBlock
    repeatBody <- codeGenProg act
    br condBlock
    -- repeat-cond
    setBlock condBlock
    cond <- cgenExpr repCond
    test <- isFalse cond
    cbr test repeatBlock exitBlock
    -- exit block
    setBlock exitBlock
    return ()

codeGenStmt Rum.WhileDo{..} = do
    condBlock  <- addBlock "while.cond"
    whileBlock <- addBlock "while.loop"
    exitBlock  <- addBlock "while.end"

    br condBlock
    -- while-cond
    setBlock condBlock
    cond <- cgenExpr whileCond
    test <- isTrue cond
    cbr test whileBlock exitBlock
    -- while-true
    setBlock whileBlock
    whileBody <- codeGenProg act
    br condBlock
    -- Exit block
    setBlock exitBlock
    return ()

codeGenStmt Rum.For{..} = do
    startBlock    <- addBlock "for.start"
    condBlock     <- addBlock "for.cond"
    doUpdateBlock <- addBlock "for.loop"
    exitBlock     <- addBlock "for.end"

    br startBlock
    -- Starting point
    setBlock startBlock
    codeGenProg start
    br condBlock
    -- Condition block
    setBlock condBlock
    cond <- cgenExpr expr
    test <- isTrue cond
    cbr test doUpdateBlock exitBlock
    -- for Body + Update block
    setBlock doUpdateBlock
    codeGenProg body >> codeGenProg update
    br condBlock
    -- Exit block
    setBlock exitBlock
    return ()


binOps :: Map Rum.BinOp (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binOps = Map.fromList [ (Rum.Add, iAdd), (Rum.Sub, iSub)
                      , (Rum.Mul, iMul), (Rum.Div, iDiv)
                      , (Rum.Mod, iMod)]
logicOps :: Map Rum.LogicOp (AST.Operand -> AST.Operand -> Codegen AST.Operand)
logicOps = Map.fromList [(Rum.And, lAnd), (Rum.Or, lOr)]
compOps :: Map Rum.CompOp (AST.Operand -> AST.Operand -> Codegen AST.Operand)
compOps = Map.fromList [ (Rum.Eq, iEq), (Rum.NotEq, iNeq)
                       , (Rum.Lt, iLt), (Rum.Gt, iGt)
                       , (Rum.NotGt, iNotGt), (Rum.NotLt, iNotLt)
                       ]

cgenExpr :: Rum.Expression -> Codegen AST.Operand
cgenExpr (Rum.Const (Rum.Number c)) = return $ cons (C.Int 32 (fromIntegral c))
cgenExpr (Rum.Var x) = getVar (T.unpack $ Rum.varName x) >>= load
cgenExpr (Rum.Neg e) = cgenExpr e >>= \x -> iSub iZero x
cgenExpr Rum.BinOper{..} =
  case Map.lookup bop binOps of
    Just f  -> cgenExpr l >>= \x -> cgenExpr r >>= \y -> f x y
    Nothing -> error "No such binary operator"
cgenExpr Rum.LogicOper{..} =
  case Map.lookup lop logicOps of
    Just f  -> cgenExpr l >>= \x -> cgenExpr r >>= \y -> f x y
    Nothing -> error "No such logic operator"
cgenExpr Rum.CompOper{..} =
  case Map.lookup cop compOps of
    Just f  -> cgenExpr l >>= \x -> cgenExpr r >>= \y -> f x y
    Nothing -> error "No such logic operator"
cgenExpr (Rum.FunCallExp Rum.FunCall{..}) = mapM cgenExpr args >>= \largs ->
    call (externf (AST.Name $ T.unpack $ Rum.varName fName)) largs


-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codeGenMaybeWorks :: String -> Rum.Program -> IO AST.Module
codeGenMaybeWorks moduleName program = withContext $ \context ->
  liftError $ withModuleFromAST context llvmAST $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return llvmAST
  where
    llvmModule    = emptyModule moduleName
    generatedLLVM = codeGenAll program
    llvmAST       = runLLVM llvmModule generatedLLVM

codegenLLVM :: String -> Rum.Program -> AST.Module
codegenLLVM moduleName program = llvmAST
  where
    generatedLLVM  = codeGenAll program
    providedModule = emptyModule moduleName
    llvmAST        = runLLVM providedModule generatedLLVM