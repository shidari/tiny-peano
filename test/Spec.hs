{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Nat (Nat(..), add, sub, toNat, fromNat)
import Lib (interpret)

instance Arbitrary Nat where
  arbitrary = toNat . getNonNegative <$> arbitrary

main :: IO ()
main = hspec $ do
  describe "Nat" $ do
    describe "add" $ do
      prop "左単位元: 0 + n = n" $ \n ->
        add Zero n `shouldBe` n
      prop "右単位元: n + 0 = n" $ \n ->
        add n Zero `shouldBe` n
      prop "結合律: (a + b) + c = a + (b + c)" $ \a b c ->
        add (add a b) c `shouldBe` add a (add b c)
      prop "可換律: a + b = b + a" $ \a b ->
        add a b `shouldBe` add b a
      prop "後者との関係: a + S(b) = S(a + b)" $ \a b ->
        add a (Succ b) `shouldBe` Succ (add a b)
      prop "fromNat で整合: fromNat (add a b) = fromNat a + fromNat b" $ \a b ->
        fromNat (add a b) `shouldBe` fromNat a + fromNat b

    describe "sub" $ do
      prop "n - 0 = n" $ \n ->
        sub n Zero `shouldBe` Right n
      prop "n - n = 0" $ \n ->
        sub n n `shouldBe` Right Zero
      prop "a >= b なら fromNat で整合" $ \a b ->
        let (x, y) = if a >= b then (a, b) else (b, a)
        in fmap fromNat (sub x y) `shouldBe` Right (fromNat x - fromNat y)
      prop "a < b なら Left" $ \a b ->
        a < b ==> case sub a b of
          Left _  -> True
          Right _ -> False

    describe "toNat / fromNat" $ do
      prop "roundtrip" $ \(NonNegative n) ->
        fromNat (toNat n) `shouldBe` n
      prop "toNat は構造を保存: toNat (n+1) = Succ (toNat n)" $ \(NonNegative n) ->
        toNat (n + 1) `shouldBe` Succ (toNat (n :: Integer))

  describe "interpret" $ do
    prop "単一の数値" $ \(NonNegative (a :: Integer)) ->
      interpret (show a) `shouldBe` show a
    prop "二項の足し算" $ \(NonNegative a) (NonNegative b) ->
      let input = show a ++ " + " ++ show b
      in interpret input `shouldBe` show (a + b :: Integer)
    prop "三項の足し算" $ \(NonNegative a) (NonNegative b) (NonNegative c) ->
      let input = show a ++ " + " ++ show b ++ " + " ++ show c
      in interpret input `shouldBe` show (a + b + c :: Integer)
    prop "括弧つき" $ \(NonNegative a) (NonNegative b) (NonNegative c) ->
      let input = "(" ++ show a ++ " + " ++ show b ++ ") + " ++ show c
      in interpret input `shouldBe` show (a + b + c :: Integer)
    prop "引き算 (a >= b)" $ \(NonNegative (a :: Integer)) (NonNegative b) ->
      a >= b ==>
        let input = show a ++ " - " ++ show b
        in interpret input `shouldBe` show (a - b)
    prop "負になる引き算は Eval error" $ \(NonNegative (a :: Integer)) (NonNegative b) ->
      a < b ==>
        let input = show a ++ " - " ++ show b
        in interpret input `shouldStartWith` "Eval error"
