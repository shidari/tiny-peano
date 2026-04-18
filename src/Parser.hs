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
  | Div Expr Expr -- 割り算
  deriving (Show, Eq)

-- パーサー本体
-- expr    = term ((('+' | '-') term)*)
-- term    = primary ((('*' | '/') primary)*)
-- primary = number | '(' expr ')'
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (skipSpace *> pExpr <* eof) ""
  where
    pExpr = do
      first <- pTerm
      rest  <- many ((,) <$> pOp <*> pTerm)
      pure $ foldl (\acc (op, t) -> op acc t) first rest
      where
        pOp = (Add <$ symbol "+") <|> (Sub <$ symbol "-")

    pTerm = do
      first <- pPrimary
      rest  <- many ((,) <$> pOp <*> pPrimary)
      pure $ foldl (\acc (op, p) -> op acc p) first rest
      where
        pOp = (Mul <$ symbol "*") <|> (Div <$ symbol "/")

    pPrimary = pParens <|> pNumber
      where
        pParens = symbol "(" *> pExpr <* symbol ")"
        pNumber = Lit <$> lexeme L.decimal

