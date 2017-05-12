{-# LANGUAGE RecordWildCards #-}

module Compiler.Rum.ToString where

import Compiler.Rum.Structure

tab :: Int -> String
tab n = '\n' : replicate (2*n) ' '

progToStr :: Int -> Program -> String
progToStr _ [] = ""
progToStr n [x] = stmtToStr n x
progToStr n (x1:x2:xs) = stmtToStr n x1 ++ ";" ++ progToStr n (x2:xs)


stmtToStr :: Int -> Statement -> String
stmtToStr n Assignment{..}  = tab n ++ name var ++ " := " ++ exprToStr value
stmtToStr n Skip            = tab n ++ "skip "
stmtToStr n IfElse{..}      = tab n ++ "if"   ++ tab (succ n) ++ exprToStr ifCond ++
                              tab n ++ "then" ++ progToStr (succ n) trueAct ++
                              tab n ++ "else" ++ progToStr (succ n) falseAct ++
                              tab n ++ "fi"
stmtToStr n RepeatUntil{..} = tab n ++ "repeat" ++ progToStr (succ n) act ++
                              tab n ++ "until " ++ exprToStr repCond
stmtToStr n WhileDo{..}     = tab n ++ "while " ++ exprToStr whileCond ++
                              tab n ++ "do"     ++ progToStr (succ n) act ++
                              tab n ++ "od"
stmtToStr n For{..}         = tab n ++ "for " ++ progToStr (succ n) start ++
                                       ", "   ++ tab (succ n) ++ exprToStr expr ++
                                       ", "   ++ progToStr (succ n) update ++
                              tab n ++ "do"   ++ progToStr (succ n) body ++
                              tab n ++ "od"

stmtToStr n Fun{..}         = tab n ++ "fun " ++ name funName ++ "(" ++ expListStr (Var <$> params) ++ ") begin" ++
                              progToStr (succ n) funBody ++
                              tab n ++ "end"
stmtToStr n Return{..}      = tab n ++ "return " ++ exprToStr retExp
stmtToStr n (FunCallStmt f) = tab n ++ funCallToStr f

exprToStr :: Expression -> String
exprToStr (Const c)     = typeToStr c
exprToStr (Var v)       = name v
exprToStr (Neg e)       = "-(" ++ exprToStr e ++ ")"
exprToStr BinOper{..}   = paren l ++ bToStr bop ++ paren r
exprToStr LogicOper{..} = paren l ++ lToStr lop ++ paren r
exprToStr CompOper{..}  = paren l ++ cToStr cop ++ paren r
exprToStr (FunCallExp f)= funCallToStr f

funCallToStr :: FunCall -> String
funCallToStr FunCall{..}   = name fName ++ "(" ++ expListStr args ++ ")"

typeToStr :: Type -> String
typeToStr (Number n) = show n
typeToStr (Str s) = s
typeToStr Unit = "()"

expListStr :: [Expression] -> String
expListStr [] = ""
expListStr [x] = exprToStr x
expListStr (x1:x2:xs) = exprToStr x1 ++", " ++ exprToStr x2 ++ expListStr xs

paren :: Expression -> String
paren e@(Const _) = exprToStr e
paren e@(Var _)   = exprToStr e
paren x           = "(" ++ exprToStr x ++ ")"

bToStr :: BinOp -> String
bToStr Add = " + "
bToStr Sub = " - "
bToStr Mul = " * "
bToStr Div = " / "
bToStr Mod = " % "
bToStr Pow = " ^ "

cToStr :: CompOp -> String
cToStr Eq    = " == "
cToStr NotEq = " != "
cToStr Lt    = " < "
cToStr NotGt = " <= "
cToStr Gt    = " > "
cToStr NotLt = " >= "

lToStr :: LogicOp -> String
lToStr And = " && "
lToStr Or  = " || "
lToStr Xor = " !! "
