{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( Expr(..)
    , parseExpr
    ) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, parse, eof, many, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L

import Lexer (lexeme, symbol, skipSpace)

-- 算術式の AST
data Expr
  = Lit Integer   -- 数値リテラル
  | Add Expr Expr -- 足し算
  | Sub Expr Expr -- 引き算
  | Mul Expr Expr -- 掛け算
  deriving (Show, Eq)

-- パーサー本体
-- expr   = term ((('+' | '-') term)*)
-- term   = factor (('*' factor)*)
-- factor = number | '(' expr ')'
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (skipSpace *> pExpr <* eof) ""
  where
    pExpr = do
      first <- pTerm
      rest  <- many pOpTerm
      pure $ foldl (\acc (op, t) -> op acc t) first rest
      where
        pOpTerm = do
          op <- (Add <$ symbol "+") <|> (Sub <$ symbol "-")
          t  <- pTerm
          pure (op, t)

    pTerm = do
      first <- pFactor
      rest  <- many pOpFactor
      pure $ foldl (\acc (op, f) -> op acc f) first rest
      where
        pOpFactor = do
          op <- Mul <$ symbol "*"
          f  <- pFactor
          pure (op, f)

    pFactor = pParens <|> pNumber
      where
        pParens = symbol "(" *> pExpr <* symbol ")"
        pNumber = Lit <$> lexeme L.decimal

