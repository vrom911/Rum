module Compiler.Rum.Internal.ExprParser where

import qualified Data.HashMap.Strict as HM
import           Data.List (foldl')
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Compiler.Rum.Internal.AST

strSpace :: String -> Parser String
strSpace s = string s >>= \x -> space >> return x

chSpace :: Char -> Parser Char
chSpace s = char s <* space

keyWords :: [String]
keyWords = ["skip", "write", "if", "then", "else", "fi", "repeat", "until", "do", "od", "while", "for"]

varNameP :: Parser Variable
varNameP = Variable <$> ((((:) <$> (try (oneOf "_$") <|> letterChar)
                        <*> many (try alphaNumChar <|> oneOf "_-$")) >>= \x -> if x `elem` keyWords
                        then fail "Can not use Key words as variable names"
                        else pure x
                        ) <* space)

parens :: Parser a -> Parser a
parens = between (chSpace '(') (chSpace ')')

rightAssocsP :: (a -> a -> a) -> Parser op -> Parser a -> Parser a
rightAssocsP f opP elP = do
    el   <- elP
    rest <- many (opP *> rightAssocsP f opP elP)
    pure $ if null rest then el else foldl' f el rest

leftAssocsP :: (a -> a -> a) -> Parser op -> Parser a -> Parser a
leftAssocsP f opP elP = elP >>= rest
  where
    rest x  =  opP *> elP >>= \y -> rest' (f x y)
    rest' x = (opP *> elP >>= \y -> rest' (f x y)) <|> pure x

basicExprP :: Parser Expression
basicExprP =   Const <$> numP
           <|> ReadLn <$ strSpace "read()"
           <|> Var <$> varNameP
           <|> parens exprP
  where
    numP :: Parser Int
    numP = (read <$> (try ((:) <$> char '-' <*> some digitChar) <|> some digitChar)) <* space

arithmeticExprP :: Parser Expression
arithmeticExprP = prior3
  where
    powP    = rightAssocsP (BinOper Pow) (chSpace '^') basicExprP
    p2 c op = leftAssocsP  (BinOper c)   (chSpace op)  powP
    prior2  = try (p2 Mul '*') <|> try (p2 Div '/') <|> try (p2 Mod '%') <|> powP
    p3 c op = leftAssocsP  (BinOper c)   (chSpace op)  prior2
    prior3  = try (p3 Add '+') <|> try (p3 Sub '-') <|> prior2

compExprP :: Parser Expression
compExprP = do
    le <- arithmeticExprP
    op <- choice (strSpace <$> ["==", "!=", "<=", "<", ">=", ">"])
    re <- arithmeticExprP
    return $ CompOper ((\(Just s) -> s) $ HM.lookup op compMap) le re
  where
    compMap = HM.fromList [("==", Eq), ("!=", NotEq), ("<=", NotGt), ("<", Lt),  (">=", NotLt), (">", Gt)]

binExprP :: Parser Expression
binExprP = try (parens compExprP <|> compExprP) <|> parens arithmeticExprP <|> arithmeticExprP

logicExprP :: Parser Expression
logicExprP = try lOr <|> try lAnd <|> binExprP
  where
    lAnd = leftAssocsP (LogicOper And) (strSpace "&&") binExprP
    lOr  = leftAssocsP (LogicOper Or)  (strSpace "||") lAnd

exprP :: Parser Expression
exprP = try (parens exprP) <|> try (parens logicExprP <|> logicExprP) <|> parens binExprP
