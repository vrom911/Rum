module Compiler.Rum.StackMachine.Stacker where


import           Control.Monad.Extra (concatMapM)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bool                 (bool)
import qualified Data.HashMap.Strict as HM

import Compiler.Rum.Internal.AST
import Compiler.Rum.StackMachine.Structure
import Compiler.Rum.StackMachine.SMUtil


newLabel :: State Int LabelId
newLabel = get >>= \x -> modify (+1) >> return (buildLabel x)

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
translateStmt Fun {..} = translate funBody >>= \f -> pure $ (Label $ LabelId $ name funName) : map Store (reverse params) ++ f
translateStmt (FunCallStmt f) = translateFunCall f
translateStmt e = error $ "Not supported operation for stack: " ++ show e

translateExpr :: Expression -> Instructions
translateExpr (Const x)     = pure [Push x]
translateExpr (Var v)       = pure [Load v]
translateExpr BinOper{..}   = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SBinOp bop]
translateExpr CompOper{..}  = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SCompOp cop]
translateExpr LogicOper{..} = translateExpr l >>= \x -> translateExpr r >>= \y -> pure $ x ++ y ++ [SLogicOp lop]
translateExpr (Neg e) = translateExpr e >>= \x -> pure $ Push (Number 0) : x ++ [SBinOp Sub]
translateExpr (FunCallExp f) = translateFunCall f
translateExpr e = error $ "Not supported operation for stack: " ++ show e

-- f :: a -> m b
-- l :: [a]
-- mapM f l :: m [b]
-- almostResult = mapM translateExpr (args call) :: m [[Instruction]]
-- result = concat <$> almostResult
-- result = concatMapM translateExpr (args call) :: m [Instruction]
translateFunCall :: FunCall -> Instructions
translateFunCall call = let funName = name $ fName call in
    concatMapM translateExpr (args call) >>= \res -> pure $ res ++
        case HM.lookup funName rumludeFunNames of
            Nothing -> [SFunCall $ LabelId funName]
            Just x  -> [SRumludeCall x]



--execute :: [Instruction] -> StateT StackEnvironment (MaybeT IO) Type
--execute = foldr ((++) . executeInstr) [return Unit]

executeInstr :: Instruction -> StateT StackEnvironment (MaybeT IO) Type
executeInstr Nop = return Unit
executeInstr (Push x) = modify (pushStack x) >> pure Unit
executeInstr Pop = modify popStack >> pure Unit
--executeInstr Store v =
executeInstr (SBinOp b) =
    gets takeStack >>= \(Just (Number y)) ->
        modify popStack >> gets takeStack >>= \(Just (Number x)) ->
            modify popStack >> modify (pushStack $ Number (binOp b x y)) >> return Unit
executeInstr (SLogicOp l) =
    gets takeStack >>= \(Just (Number y)) ->
        modify popStack >> gets takeStack >>= \(Just (Number x)) ->
            modify popStack >> modify (pushStack $ Number (logicOp l x y)) >> return Unit
executeInstr (SCompOp c) =
    gets takeStack >>= \(Just y) ->
        modify popStack >> gets takeStack >>= \(Just x) ->
            modify popStack >> modify (pushStack (intCompare x y)) >> return Unit
            where
                intCompare :: Type -> Type -> Type
                intCompare x y = Number $ bool 0 1 $ compOp c x y
