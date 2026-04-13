module Eval
    ( eval
    ) where

import Nat (Nat(..), add, toNat)
import Parser (Expr(..))

eval :: Expr -> Nat
eval (Lit n)   = toNat n
eval (Add a b) = add (eval a) (eval b)
