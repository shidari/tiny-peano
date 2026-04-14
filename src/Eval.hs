module Eval
    ( eval
    ) where

import Nat (Nat, add, sub, toNat)
import Parser (Expr(..))

eval :: Expr -> Either String Nat
eval (Lit n)   = Right (toNat n)
eval (Add a b) = add <$> eval a <*> eval b
eval (Sub a b) = do
  x <- eval a
  y <- eval b
  sub x y
