module Compiler.Rum.Parser where


import Control.Applicative    ((<|>), liftA2)
import Text.Megaparsec        ( alphaNumChar, between, char
                              , digitChar, letterChar, many
                              , notFollowedBy, oneOf, option
                              , sepBy, some, space, string, try
                              )
import Text.Megaparsec.Expr   (Operator(..), makeExprParser)
import Text.Megaparsec.String (Parser)

import Compiler.Rum.Structure

strSpace :: String -> Parser String
strSpace s = string s >>= \x -> space >> return x

chSpace :: Char -> Parser Char
chSpace s = char s <* space

keyword :: String -> Parser ()
keyword w = string w *> notFollowedBy alphaNumChar *> space

keyWords :: [String]
keyWords = ["skip", "if", "then", "else", "fi", "repeat", "until", "do", "od", "while", "for", "fun", "begin", "end", "return"]

numP :: Parser Int
numP = (read <$> some digitChar) <* space

varNameP :: Parser Variable
varNameP = Variable <$> ((((:) <$> (try (oneOf "_$") <|> letterChar)
                        <*> many (try alphaNumChar <|> oneOf "_$")) >>= \x -> if x `elem` keyWords
                        then fail "Can not use Key words as variable names"
                        else pure x
                        ) <* space)

paramsP :: Parser [Variable]
paramsP = varNameP `sepBy` chSpace ','

parens :: Parser a -> Parser a
parens = between (chSpace '(') (chSpace ')')

funCallP :: (FunCall -> f) -> Parser f
funCallP funCtor = do
    n <- varNameP
    e <- parens (exprP `sepBy` chSpace ',')
    return $ funCtor $ FunCall n e

aOperators :: [[Operator Parser Expression]]
aOperators =
  [ [ Prefix (Neg <$ chSpace '-')
    ]
  , [ InfixR (BinOper Pow <$ chSpace '^')]
  , [ InfixL (BinOper Mul <$ chSpace '*')
    , InfixL (BinOper Div <$ chSpace '/')
    , InfixL (BinOper Mod <$ chSpace '%')
    ]
  , [ InfixL (BinOper Add <$ chSpace '+')
    , InfixL (BinOper Sub <$ chSpace '-')
    ]
  , [ InfixN (CompOper Eq    <$ strSpace "==")
    , InfixN (CompOper NotEq <$ strSpace "!=")
    , InfixN (CompOper NotGt <$ strSpace "<=")
    , InfixN (CompOper Lt    <$ strSpace "<")
    , InfixN (CompOper NotLt <$ strSpace ">=")
    , InfixN (CompOper Gt    <$ strSpace ">")
    ]
  , [ InfixL (LogicOper And <$ strSpace "&&")
    ]
  , [ InfixL (LogicOper Or  <$ strSpace "||")
    , InfixL (LogicOper Xor <$ strSpace "!!")
    ]
  ]

basicExprP :: Parser Expression
basicExprP =   parens arithmeticExprP
           <|> Const . Number <$> numP
           <|> try (funCallP FunCallExp)
           <|> Var     <$> varNameP

arithmeticExprP, exprP:: Parser Expression
arithmeticExprP = makeExprParser basicExprP aOperators
exprP           = arithmeticExprP

----------------------
-- Statements
----------------------
semiSep :: Parser a -> Parser [a]
semiSep p = p `sepBy` chSpace ';'

progMainP :: Parser Program
progMainP = space *> semiSep stmtP

betweenDo :: Parser a -> Parser a
betweenDo = between (keyword "do") (keyword "od")

stmtP :: Parser Statement
stmtP =   parens stmtP
      <|> skipP
      <|> ifP
      <|> repeatP
      <|> whileP
      <|> forP
      <|> returnP
      <|> try assignP
      <|> funCallP FunCallStmt
  where
    assignP :: Parser Statement
    assignP = do
        v <- varNameP <* strSpace ":="
        e <- exprP
        return $ Assignment v e

    skipP :: Parser Statement
    skipP = Skip <$ keyword "skip"

    ifP :: Parser Statement
    ifP = do
        cond  <- keyword "if"   *> exprP
        true  <- keyword "then" *> progMainP
        false <- option [Skip] (keyword "else" *> progMainP)
        ()    <$ keyword "fi"
        return $ IfElse cond true false

    repeatP :: Parser Statement
    repeatP = do
        a    <- keyword "repeat" *> progMainP
        cond <- keyword "until"  *> exprP
        return $ RepeatUntil cond a

    whileP :: Parser Statement
    whileP = do
        cond <- keyword "while" *> exprP
        a    <- betweenDo progMainP
        return $ WhileDo cond a

    forP :: Parser Statement
    forP = do
        s <- keyword "for" *> progMainP
        e <- strSpace ","   *> exprP
        u <- strSpace ","   *> progMainP
        b <- betweenDo progMainP
        return $ For s e u b

    returnP :: Parser Statement
    returnP = Return <$> (keyword "return" *> exprP)

funP :: Parser Statement
funP = do
    n    <- keyword "fun" *> varNameP
    prms <- parens paramsP
    b    <- keyword "begin" *> progMainP <* keyword "end"
    return $ Fun n prms b

progP :: Parser Program
progP = liftA2 (++) (space *> many (funP <* space)) progMainP