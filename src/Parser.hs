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
  deriving (Show, Eq)

-- パーサー本体
-- expr = term ((('+' | '-') term)*)
-- term = number | '(' expr ')'
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (skipSpace *> pExpr <* eof) ""
  where
    pExpr = do
      first <- pTerm
      rest  <- many pOpTerm
      pure $ foldl (\acc (op, t) -> op acc t) first rest

    pOpTerm = do
      op <- (Add <$ symbol "+") <|> (Sub <$ symbol "-")
      t  <- pTerm
      pure (op, t)

    pTerm = pParens <|> pNumber

    pParens = symbol "(" *> pExpr <* symbol ")"

    pNumber = Lit <$> lexeme L.decimal

