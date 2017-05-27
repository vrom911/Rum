module Compiler.Rum.Internal.Parser where


import           Control.Applicative    ((<|>), liftA2)
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text as T
import           Text.Megaparsec        ( anyChar, alphaNumChar, between, char
                                        , digitChar, letterChar, many, manyTill
                                        , notFollowedBy, oneOf, option, optional
                                        , sepBy, some, space, string, try
                                        )
import Text.Megaparsec.Expr             (Operator(..), makeExprParser)
import Text.Megaparsec.Text             (Parser)

import Compiler.Rum.Internal.AST

text :: String -> Parser Text
text t = T.pack <$> string t

txtSpace :: String -> Parser Text
txtSpace s = text s >>= \x -> space >> return x

chSpace :: Char -> Parser Char
chSpace s = char s <* space

keyword :: String -> Parser ()
keyword w = text w *> notFollowedBy alphaNumChar *> space

keyWords :: [String]
keyWords = [ "skip", "if", "then", "else", "fi", "repeat"
           , "until", "do", "od", "while", "for", "fun"
           , "begin", "end", "return", "true", "false"
           ]

numP :: Parser Int
numP = (read <$> some digitChar) <* space

charP :: Parser Char
charP = between (char '\'') (char '\'') anyChar <* space

txtP :: Parser Text
txtP = T.pack <$> (char '"' *> anyChar `manyTill` char '"' <* space)

boolP :: Parser Int
boolP = (1 <$ text "true" <|> 0 <$ text "false") <* space

varNameP :: Parser Variable
varNameP = fromString <$> (((:) <$> (try (oneOf ['_','$']) <|> letterChar)
                                <*> many (try alphaNumChar <|> oneOf ['_','$'])) >>= \x -> if x `elem` keyWords
                                then fail "Can not use Key words as variable names"
                                else pure x) <* space

varArrFuncallNameP :: Parser Expression
varArrFuncallNameP = do
    wtfName <- varNameP
    maybeArr <- optional $ some $ between (txtSpace "[") (txtSpace "]") exprP
    case maybeArr of
        Just arrExp -> return $ ArrC $ ArrCell wtfName arrExp
        Nothing -> do
            maybeFun <- optional $ parens (exprP `sepBy` chSpace ',')
            return $ case maybeFun of
                Just ex -> FunCallExp $ FunCall wtfName ex
                Nothing -> Var wtfName

varOrArrP :: Parser Expression
varOrArrP = do
    arName <- varNameP
    ex <- optional $ some $ between (txtSpace "[") (txtSpace "]") exprP
    case ex of
        Nothing -> return $ Var arName
        Just exs -> return $ ArrC $ ArrCell arName exs

arrP :: Parser [Expression]
arrP = between (txtSpace "[") (txtSpace "]") (exprP `sepBy` chSpace ',')

emptyArrP :: Parser [Expression]
emptyArrP = [] <$ txtSpace "{}"

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
  , [ InfixN (CompOper Eq    <$ txtSpace "==")
    , InfixN (CompOper NotEq <$ txtSpace "!=")
    , InfixN (CompOper NotGt <$ txtSpace "<=")
    , InfixN (CompOper Lt    <$ txtSpace "<")
    , InfixN (CompOper NotLt <$ txtSpace ">=")
    , InfixN (CompOper Gt    <$ txtSpace ">")
    ]
  , [ InfixL (LogicOper And <$ txtSpace "&&")
    ]
  , [ InfixL (LogicOper Or  <$ txtSpace "||")
    , InfixL (LogicOper Xor <$ txtSpace "!!")
    ]
  ]

basicExprP :: Parser Expression
basicExprP =   parens arithmeticExprP
           <|> Const . Number <$> numP
           <|> Const . Ch     <$> charP
           <|> Const . Str    <$> txtP
           <|> Const . Number <$> boolP
           <|> ArrLit         <$> (arrP <|> emptyArrP)
           <|> varArrFuncallNameP

arithmeticExprP, exprP :: Parser Expression
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
      <|> funCallStmtP
  where
    assignP :: Parser Statement
    assignP = do
        v <- varOrArrP <* txtSpace ":="
        e <- exprP
        return $ case v of
            Var v'  -> AssignmentVar v' e
            ArrC ar -> AssignmentArr ar e
            _       -> error "Assignment left part fail"

    skipP :: Parser Statement
    skipP = Skip <$ keyword "skip"

    ifP :: Parser Statement
    ifP = keyword "if" *> ifHelper <* keyword "fi"

    ifHelper :: Parser Statement
    ifHelper = do
        cond  <- exprP
        true  <- keyword "then" *> progMainP
        false <- try (keyword "elif" *> ((:[]) <$> ifHelper)) <|> option [Skip] (keyword "else" *> progMainP)
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
        e <- txtSpace ","  *> exprP
        u <- txtSpace ","  *> progMainP
        b <- betweenDo progMainP
        return $ For s e u b

    returnP :: Parser Statement
    returnP = Return <$> (keyword "return" *> exprP)

    funCallStmtP :: Parser Statement
    funCallStmtP = do
        fs@(FunCallStmt _) <- funCallP FunCallStmt
--        let funname = fName f
--        if funname == "strset" then
--            let Var v = head (args f) in
--            return $ AssignmentVar v (FunCallExp f)
--        else
        return fs

funP :: Parser Statement
funP = do
    n    <- keyword "fun" *> varNameP
    prms <- parens paramsP
    b    <- keyword "begin" *> progMainP <* keyword "end"
    return $ Fun n prms b

progP :: Parser Program
progP = liftA2 (++) (space *> many (funP <* space)) progMainP