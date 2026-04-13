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
  deriving (Show, Eq)

-- パーサー本体
-- expr = term (('+' term)*)
-- term = number | '(' expr ')'
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (skipSpace *> pExpr <* eof) ""
  where
    pExpr = do
      first <- pTerm
      rest  <- many (symbol "+" *> pTerm)
      pure $ foldl Add first rest

    pTerm = pParens <|> pNumber

    pParens = symbol "(" *> pExpr <* symbol ")"

    pNumber = Lit <$> lexeme L.decimal

