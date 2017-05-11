module Compiler.Rum.Parser where

-- TODO: перенести всё в модуль Compiler
--    * Compiler.Language.Structure
import Control.Applicative    ((<|>))
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

kword :: String -> Parser ()
kword w = string w *> notFollowedBy alphaNumChar *> space

keyWords :: [String]
keyWords = ["skip", "write", "if", "then", "else", "fi", "repeat", "until", "do", "od", "while", "for"]

numP :: Parser Int
numP = (read <$> some digitChar) <* space

varNameP :: Parser Variable
varNameP = Variable <$> ((((:) <$> (try (oneOf "_$") <|> letterChar)
                        <*> many (try alphaNumChar <|> oneOf "_$")) >>= \x -> if x `elem` keyWords
                        then fail "Can not use Key words as variable names"
                        else pure x
                        ) <* space)

parens :: Parser a -> Parser a
parens = between (chSpace '(') (chSpace ')')

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
           <|> Const <$> numP
           <|> ReadLn <$ strSpace "read()"
           <|> Var <$> varNameP

arithmeticExprP, exprP:: Parser Expression
arithmeticExprP = makeExprParser basicExprP aOperators
exprP           = arithmeticExprP

----------------------
-- Statements
----------------------
semiSep :: Parser a -> Parser [a]
semiSep p = p `sepBy` chSpace ';'

progP :: Parser Program
progP = space *> semiSep stmtP

betweenDo :: Parser a -> Parser a
betweenDo = between (kword "do") (kword "od")

stmtP :: Parser Statement
stmtP =   parens stmtP
      <|> writeP
      <|> skipP
      <|> ifP
      <|> repeatP
      <|> whileP
      <|> forP
      <|> assignP
  where
    assignP :: Parser Statement
    assignP = do
        v <- varNameP <* strSpace ":="
        e <- exprP
        return $ Assignment v e

    writeP :: Parser Statement
    writeP = do
        e <- kword "write" *> parens exprP
        return $ WriteLn e

    skipP :: Parser Statement
    skipP = Skip <$ kword "skip"

    ifP :: Parser Statement
    ifP = do
        cond  <- kword "if"   *> exprP
        true  <- kword "then" *> progP
        false <- option [Skip] (kword "else" *> progP)
        ()    <$ kword "fi"
        return $ IfElse cond true false

    repeatP :: Parser Statement
    repeatP = do
        a    <- kword "repeat" *> progP
        cond <- kword "until"  *> exprP
        return $ RepeatUntil cond a

    whileP :: Parser Statement
    whileP = do
        cond <- kword "while" *> exprP
        a    <- betweenDo progP
        return $ WhileDo cond a

    forP :: Parser Statement
    forP = do
        s <- kword "for" *> progP
        e <- strSpace ","   *> exprP
        u <- strSpace ","   *> progP
        b <- betweenDo progP
        return $ For s e u b
