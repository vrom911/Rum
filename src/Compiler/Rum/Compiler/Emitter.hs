module Compiler.Rum.Compiler.Emitter where

import           Control.Monad.Except (ExceptT, forM_, runExceptT, (>=>))
import           Control.Monad.State
import           Data.Char            (isUpper, ord)
import           Data.Map             (Map)
import qualified Data.Map as Map      (fromList, lookup)
import qualified Data.Text as T

import qualified LLVM.AST as AST            (Operand(..), Type(..), Module(..), Name(..), Operand(..))
import qualified LLVM.AST.Constant as C     (Constant(..))
import qualified LLVM.AST.Type     as Ty
import           LLVM.Context               (withContext)
import           LLVM.Module                (moduleLLVMAssembly, withModuleFromAST)

import           Compiler.Rum.Compiler.CodeGen
import qualified Compiler.Rum.Internal.AST as Rum

toSig :: [(Rum.Variable, Rum.DataType)] -> [(AST.Type, AST.Name)]
toSig = let nm = T.unpack . Rum.varName in
        map (\(x, y) -> (fromDataToType y, AST.Name (nm x)))

-- Fun declarations + main body
codeGenAll :: Rum.Program -> LLVM ()
codeGenAll pr = let (funs, main) = span isFunDeclSt pr in
    codeGenTops funs >> codeGenMain main
  where
    isFunDeclSt :: Rum.Statement -> Bool
    isFunDeclSt Rum.Fun{} = True
    isFunDeclSt _     = False

-- Deal with std funs
codegenProgram :: Rum.Program -> LLVM ()
codegenProgram program = do
--    defineIOStrVariable ".scanf_str"  "%d\0"
--    defineIOStrVariable ".printf_str" "%d\n\0"

    codeGenAll program

    declareExtFun Ty.i32 "rumRead"  [] False
    declareExtFun Ty.i32 "rumWrite" [(Ty.i32, AST.Name "")] False
    declareExtFun Ty.i32 "rumStrlen" [(Ty.ptr Ty.i8, AST.Name "")] False
    declareExtFun Ty.i32 "rumStrget" [(Ty.ptr Ty.i8, AST.Name ""), (Ty.i32, AST.Name "")] False
    declareExtFun Ty.i32 "rumStrcmp" [(Ty.ptr Ty.i8, AST.Name ""), (Ty.ptr Ty.i8, AST.Name "")] False
    declareExtFun Ty.i32 "rumArrlen" [(Ty.ptr Ty.i8, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i8) "rumStrsub" [(Ty.ptr Ty.i8, AST.Name ""), (Ty.i32, AST.Name ""), (Ty.i32, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i8) "rumStrdup" [(Ty.ptr Ty.i8, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i8) "rumStrset" [(Ty.ptr Ty.i8, AST.Name ""), (Ty.i32, AST.Name ""), (Ty.i8, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i8) "rumStrcat" [(Ty.ptr Ty.i8, AST.Name ""), (Ty.ptr Ty.i8, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i8) "rumStrmake" [(Ty.i32, AST.Name ""), (Ty.i8, AST.Name "")] False
    declareExtFun (Ty.ptr Ty.i32) "rumarrmake" [(Ty.i32, AST.Name ""), (Ty.i32, AST.Name "")] False
--    declareExtFun Ty.i32 "scanf"   [(Ty.ptr Ty.i8, AST.Name "")] True
--    declareExtFun Ty.i32 "printf"  [(Ty.ptr Ty.i8, AST.Name "")] True
--    declareExtFun Ty.i64 "strlen"  [(Ty.ptr Ty.i8, AST.Name "")] False

-- Declaration of many funs
codeGenTops :: Rum.Program -> LLVM AST.Operand
codeGenTops = foldr ((>>) . codeGenTop) (return iZero)

-- Deal with one fun declaration in the beginning of the file
codeGenTop :: Rum.Statement -> LLVM ()
codeGenTop Rum.Fun{..} = defineFun (fromDataToType retType) (Rum.varName funName) fnargs bls
  where
    p = map fst params
    fnargs = toSig params
    bls = createBlocks $ execCodegen $ do
      entr <- addBlock entryBlockName
      setBlock entr
      forM_ params $ \(a, b) -> do
        let aName = T.unpack $ Rum.varName a
        let t = fromDataToType b
        var <- alloca t
        () <$ store var (local t (AST.Name aName))
        assign aName var
      codeGenFunProg funBody >>= ret
codeGenTop _ = error "Impossible happened in CodeGenTop. Only fun Declarations allowed!"

-- Deal with stmts after all fun declarations (main)
codeGenMain :: Rum.Program -> LLVM ()
codeGenMain pr = defineFun iType "main" [] bls
  where
    bls = createBlocks $ execCodegen $ do
        entr <- addBlock "main"
        setBlock entr
        codeGenProg pr
        ret iZero

-- This one is for Fun declarations (should have return value)
codeGenFunProg :: Rum.Program -> Codegen AST.Operand
codeGenFunProg []                 = pure iZero
codeGenFunProg (Rum.Return{..}:_) = cgenExpr retExp
codeGenFunProg (s:stmts)          = codeGenStmt s >> codeGenFunProg stmts

-- Main prog
codeGenProg :: Rum.Program -> Codegen ()
codeGenProg []                 = pure ()
codeGenProg (Rum.Return{..}:_) = cgenExpr retExp >>= ret >> pure ()
codeGenProg (s:stmts)          = codeGenStmt s >> codeGenProg stmts

codeGenStmt :: Rum.Statement -> Codegen ()
codeGenStmt Rum.Skip = return ()
codeGenStmt Rum.Return{..} = cgenExpr retExp >>= ret >> return ()
codeGenStmt Rum.AssignmentVar{..} = do
    cgenedVal <- cgenExpr value
    symTabl <- gets symTable
    let vars = map fst symTabl
    let vName = T.unpack $ Rum.varName var
    if vName `elem` vars
    then do
        oldV <- getVar vName
        () <$ store oldV cgenedVal
    else do
        v <- alloca (typeOfOperand cgenedVal)
        () <$ store v cgenedVal
        assign vName v
codeGenStmt (Rum.AssignmentArr cell@Rum.ArrCell{..} val) = do
    cgenedVal <- cgenExpr val
    cgenedCell<- cgenExpr $ Rum.ArrC cell
    () <$ store cgenedCell cgenedVal

codeGenStmt (Rum.FunCallStmt f) =
    void $ codeGenFunCall f
codeGenStmt Rum.IfElse{..} = do
    ifTrueBlock <- addBlock "if.then"
    elseBlock   <- addBlock "if.else"
    ifExitBlock <- addBlock "if.exit"
    -- %entry
    cond <- cgenExpr ifCond
    test <- isTrue cond
    cbr test ifTrueBlock elseBlock -- Branch based on the condition
    -- if.then
    setBlock ifTrueBlock
    codeGenProg trueAct       -- Generate code for the true branch
    br ifExitBlock              -- Branch to the merge block
    -- if.else
    setBlock elseBlock
    codeGenProg falseAct       -- Generate code for the false branch
    br ifExitBlock              -- Branch to the merge block
    -- if.exit
    setBlock ifExitBlock
    return ()
codeGenStmt Rum.RepeatUntil{..} = do
    repeatBlock <- addBlock "repeat.loop"
    condBlock   <- addBlock "repeat.cond"
    exitBlock   <- addBlock "repeat.exit"

    br repeatBlock
    -- repeat-body
    setBlock repeatBlock
    codeGenProg act
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
    exitBlock  <- addBlock "while.exit"

    br condBlock
    -- while-cond
    setBlock condBlock
    cond <- cgenExpr whileCond
    test <- isTrue cond
    cbr test whileBlock exitBlock
    -- while-true
    setBlock whileBlock
    codeGenProg act
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
cgenExpr (Rum.Const (Rum.Number c)) = return $ cons (C.Int iBits (fromIntegral c))
cgenExpr (Rum.Const (Rum.Ch c))     = pure $ cons (C.Int 8 (fromIntegral $ ord c))
cgenExpr (Rum.Const (Rum.Str s))    = pure $ cons $ C.Array Ty.i8 $
                                    map (C.Int 8 . fromIntegral . ord) (T.unpack s) ++ [C.Int 8 0]
cgenExpr (Rum.ArrC Rum.ArrCell{..}) = let nm = T.unpack $ Rum.varName arr in
    mapM cgenExpr index >>= \inds -> getVar nm >>= \v -> getToCell v (map foldEx inds)
 where
   getToCell o [] = pure o
   getToCell o (x:xs) = getElementPtrInd o x >>= \op -> getToCell op xs

   foldEx (AST.ConstantOperand (C.Int 32 x)) = x
--   foldEx (AST.LocalReference t n) =

cgenExpr (Rum.ArrLit exps) = mapM cgenExpr exps >>= \cgenedE -> pure $ cons $ C.Array iType (map foldEx cgenedE)
  where
    foldEx (AST.ConstantOperand c) = c
cgenExpr (Rum.Var x) = let nm = T.unpack $ Rum.varName x in
    getVar nm >>= \v ->
        gets varTypes >>= \tps -> case Map.lookup nm tps of
            Just Ty.ArrayType{..} -> getElementPtr v
            Just _                -> load v
            Nothing               -> error "variable type is unknown"
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
cgenExpr (Rum.FunCallExp f) = codeGenFunCall f


rumFunNamesMap :: Map String (String, Ty.Type)
rumFunNamesMap = Map.fromList [ ("write",  ("rumWrite", iType)),  ("read", ("rumRead", iType))
                              , ("strlen", ("rumStrlen", iType)), ("strget", ("rumStrget", iType))
                              , ("strsub", ("rumStrsub", Ty.ptr Ty.i8)), ("strdup", ("rumStrdup", Ty.ptr Ty.i8))
                              , ("strset", ("rumStrset", Ty.ptr Ty.i8)), ("strcat", ("rumStrcat", Ty.ptr Ty.i8))
                              , ("strcmp", ("rumStrcmp", iType)), ("strmake", ("rumStrmake", Ty.ptr Ty.i8))
                              , ("arrmake", ("rumarrmake", Ty.ptr Ty.i8)), ("Arrmake", ("rumArrmake", Ty.ptr Ty.i8))
                              , ("arrlen", ("rumArrlen", iType))
                              ]

codeGenFunCall :: Rum.FunCall -> Codegen AST.Operand
codeGenFunCall Rum.FunCall{..} =
    let funNm = T.unpack $ Rum.varName fName in
    mapM modifiedCgenExpr args >>= \largs ->
            case Map.lookup funNm rumFunNamesMap of
                Just (n, t) -> call (externf t (AST.Name n)) largs
                Nothing -> call (externf iType (AST.Name funNm)) largs

modifiedCgenExpr :: Rum.Expression -> Codegen AST.Operand
modifiedCgenExpr str@(Rum.Const (Rum.Str _)) = do
    codeGenStmt (Rum.AssignmentVar "T@" str)
    getVar "T@" >>= getElementPtr
modifiedCgenExpr x = cgenExpr x

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codeGenMaybeWorks :: String -> Rum.Program -> IO AST.Module
codeGenMaybeWorks moduleName program = withContext $ \context ->
  liftError $ withModuleFromAST context llvmAST $ \m -> do
    llstr <- moduleLLVMAssembly m
    writeFile "local_example.ll" llstr
    return llvmAST
  where
    llvmModule    = emptyModule moduleName
    generatedLLVM = codegenProgram program
    llvmAST       = runLLVM llvmModule generatedLLVM