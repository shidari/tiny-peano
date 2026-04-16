module Nat
    ( Nat(..)
    , add
    , sub
    , mul
    , toNat
    , fromNat
    ) where

data Nat = Zero | Succ Nat
  deriving (Eq, Ord)

instance Show Nat where
  show n = show (fromNat n)

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

sub :: Nat -> Nat -> Either String Nat
sub n        Zero     = Right n
sub Zero     (Succ _) = Left "negative result"
sub (Succ n) (Succ m) = sub n m

mul :: Nat -> Nat -> Nat
mul Zero     _ = Zero
mul (Succ n) m = add m (mul n m)

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n - 1))

fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n
