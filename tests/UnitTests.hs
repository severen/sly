-- SPDX-FileCopyrightText: 2022 Severen Redwood <sev@severen.dev>
-- SPDX-License-Identifier: CC0-1.0

module UnitTests (unitTests) where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Sly.Eval (hnf, whnf)
import Sly.Syntax (
  Name (..),
  Term (..),
  fromChurchNat,
  toChurchNat,
  toChurchBool,
  fromChurchBool,
 )

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" do
  describe "Church Numerals" do
    it "0 == \\f x -> x" do
      let zero = Abs (Name "f") $ Abs (Name "x") (Var $ Name "x")
      toChurchNat 0 `shouldBe` zero
      fromChurchNat zero `shouldBe` Just 0

    it "1 == \\f x -> f x" do
      let one = Abs (Name "f") $ Abs (Name "x") $ App (Var $ Name "f") (Var $ Name "x")
      toChurchNat 1 `shouldBe` one
      fromChurchNat one `shouldBe` Just 1

    it "2 == \\f x -> f (f x)" do
      let two =
            Abs (Name "f") $
              Abs (Name "x") $
                App (Var $ Name "f") (App (Var $ Name "f") (Var $ Name "x"))
      toChurchNat 2 `shouldBe` two
      fromChurchNat two `shouldBe` Just 2

  describe "Church Booleans" do
    it "#t == \\t f -> t" do
      let true = Abs (Name "t") $ Abs (Name "f") $ Var (Name "t")
      toChurchBool True `shouldBe` true
      fromChurchBool true `shouldBe` Just True

    it "#f == \\t f -> f" do
      let false = Abs (Name "t") $ Abs (Name "f") $ Var (Name "f")
      toChurchBool False `shouldBe` false
      fromChurchBool false `shouldBe` Just False

  describe "Evaluator" do
    let x = (Var $ Name "x")
     in do
          it "hnf x == x" $ hnf x `shouldBe` x
          it "whnf x == x" $ whnf x `shouldBe` x

    let f = Abs (Name "x") $ App (Var $ Name "x") (Var $ Name "y")
     in do
          it "hnf (\\x -> x y) == \\x -> x y" $ hnf f `shouldBe` f
          it "whnf (\\x -> x y) == \\x -> x y" $ whnf f `shouldBe` f

    let t = App (Var $ Name "x") (Var $ Name "y")
     in do
          it "hnf (x y) == x y" $ hnf t `shouldBe` t
          it "whnf (x y) == x y" $ whnf t `shouldBe` t

    let f = Var $ Name "f"
        g = Abs (Name "x") $ App (Var $ Name "f") (Var $ Name "x")
        y = Var $ Name "y"
        t = App g y
     in do
          it "hnf ((\\x -> f x) y) == f y" $ hnf t `shouldBe` App f y
          it "whnf ((\\x -> f x) y) == f y" $ whnf t `shouldBe` App f y
