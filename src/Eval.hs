module Eval
    ( eval
    ) where

import Nat (Nat, add, sub, mul, div, toNat)
import Parser (Expr(..))
import Prelude hiding (div)

eval :: Expr -> Either String Nat
eval (Lit n)   = Right (toNat n)
eval (Add a b) = add <$> eval a <*> eval b
eval (Mul a b) = mul <$> eval a <*> eval b
eval (Sub a b) = do
  x <- eval a
  y <- eval b
  sub x y
eval (Div a b) = do
  x <- eval a
  y <- eval b
  div x y
