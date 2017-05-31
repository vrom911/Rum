module Compiler.Rum.StackMachine.Translator where

import           Control.Monad.Extra       (concatMapM)
import qualified Data.HashMap.Strict as HM

import Compiler.Rum.Internal.AST
import Compiler.Rum.Internal.Rumlude
import Compiler.Rum.Internal.Util
import Compiler.Rum.StackMachine.Structure
import Compiler.Rum.StackMachine.Util

translateP :: Program -> Instructions
translateP pr = let (funs, rest) = span isFun pr in
    translate funs >>= \f -> translate rest >>= \r -> pure $ f ++ [Label "start"] ++ r
  where
    isFun Fun{} = True
    isFun _ = False

translate :: Program -> Instructions
translate = concatMapM translateStmt

translateStmt :: Statement -> Instructions
translateStmt Skip = pure [Nop]
translateStmt AssignmentVar{..} = translateExpr value >>= \x -> pure $ x ++ [Store var]
translateStmt AssignmentArr{..} = translateExpr value >>= \x ->
    concatMapM translateExpr (index arrC) >>= \inds ->
        pure $ x ++ inds ++ [StoreArr (arr arrC) (length $ index arrC) ]
translateStmt IfElse{..} = do
    lblTrue <- newLabel
    lblFalse <- newLabel
    ifC  <- translateExpr ifCond
    fAct <- translate falseAct
    tAct <- translate trueAct
    pure $ ifC ++ [JumpIfTrue lblTrue]
               ++ fAct ++ [Jump lblFalse, Label lblTrue]
               ++ tAct ++ [Label lblFalse]
translateStmt RepeatUntil{..} = do
    lblRepeat <- newLabel
    action <- translate act
    repC <- translateExpr repCond
    pure $ Label lblRepeat:action ++ repC ++ [JumpIfFalse lblRepeat]
translateStmt WhileDo{..} = do
    lblWhile <- newLabel
    lblEnd   <- newLabel
    whileC <- translateExpr whileCond
    action <- translate act
    pure $ Label lblWhile:whileC ++ [JumpIfFalse lblEnd]
                ++ action ++ [Jump lblWhile, Label lblEnd]
translateStmt For{..} = do
    lblFor <- newLabel
    lblEnd <- newLabel
    st <- translate start
    forExp <- translateExpr expr
    bodyF <- translate body
    up <- translate update
    pure $ st ++ Label lblFor:forExp ++ [JumpIfFalse lblEnd]
              ++ bodyF ++ up ++ [Jump lblFor, Label lblEnd]

translateStmt Return{..} = translateExpr retExp >>= \ret -> pure $ ret ++ [SReturn]
translateStmt Fun {..} = translate funBody >>= \f ->
    pure $ (Label $ LabelId $ varName funName) : map Store (reverse params) ++ f
translateStmt (FunCallStmt f@FunCall{fName = "strset", args = [var@(Var v), i, c]}) = do
    str <- translateExpr var
    ind <- translateExpr i
    ch <- translateExpr c
    pure $ str ++ ind ++ ch ++ [SRumludeCall Strset, Store v]
translateStmt (FunCallStmt f) = translateFunCall f
--translateStmt e = error $ "Not supported operation for stack: " ++ show e

translateExpr :: Expression -> Instructions
translateExpr (Const x)     = pure [Push x]
translateExpr (Var v)       = if isUp v then pure [LoadRef v] else pure [Load v]
translateExpr (ArrC ArrCell{..}) = concatMapM translateExpr index >>= \indexes ->
    pure $ indexes ++ [LoadArr arr $ length indexes]
translateExpr (ArrLit exps) = concatMapM translateExpr exps >>= \ins -> pure $ ins ++ [PushNArr $ length exps]
translateExpr BinOper{..}   = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SBinOp bop]
translateExpr CompOper{..}  = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SCompOp cop]
translateExpr LogicOper{..} = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SLogicOp lop]
translateExpr (Neg e) = translateExpr e >>= \x -> pure $ Push (Number 0) : x ++ [SBinOp Sub]
translateExpr (FunCallExp f) = translateFunCall f
--translateExpr e = error $ "Not supported operation for stack: " ++ show e

-- f :: a -> m b
-- l :: [a]
-- mapM f l :: m [b]
-- almostResult = mapM translateExpr (args call) :: m [[Instruction]]
-- result = concat <$> almostResult
-- result = concatMapM translateExpr (args call) :: m [Instruction]
translateFunCall :: FunCall -> Instructions
translateFunCall FunCall{..} = let funName = varName fName in
    concatMapM translateExpr args >>= \res -> pure $ res ++
        case HM.lookup funName rumludeFunNames of
            Nothing -> [SFunCall (LabelId funName) (length args)]
            Just x  -> [SRumludeCall x]
