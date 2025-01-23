-- SPDX-FileCopyrightText: 2022 Severen Redwood <sev@severen.dev>
-- SPDX-License-Identifier: CC0-1.0

module UnitTests (unitTests) where

import Sly.Eval (hnf, whnf)
import Sly.Syntax
  ( Term (..),
    fromChurchBool,
    fromChurchNat,
    toChurchBool,
    toChurchNat,
  )
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" do
  describe "Church Numerals" do
    it "0 == \\s z -> z" do
      let zero = Abs "s" $ Abs "z" (Var "z")
      toChurchNat 0 `shouldBe` zero
      fromChurchNat zero `shouldBe` Just 0

    it "1 == \\s z -> s z" do
      let one = Abs "s" $ Abs "z" $ App (Var "s") (Var "z")
      toChurchNat 1 `shouldBe` one
      fromChurchNat one `shouldBe` Just 1

    it "2 == \\s z -> s (s z)" do
      let two =
            Abs "s" $
              Abs "z" $
                App (Var "s") (App (Var "s") (Var "z"))
      toChurchNat 2 `shouldBe` two
      fromChurchNat two `shouldBe` Just 2

  describe "Church Booleans" do
    it "#t == \\t f -> t" do
      let true = Abs "t" $ Abs "f" (Var "t")
      toChurchBool True `shouldBe` true
      fromChurchBool true `shouldBe` Just True

    it "#f == \\t f -> f" do
      let false = Abs "t" $ Abs "f" (Var "f")
      toChurchBool False `shouldBe` false
      fromChurchBool false `shouldBe` Just False

  describe "Evaluator" do
    let x = (Var "x")
     in do
          it "hnf x == x" $ hnf x `shouldBe` x
          it "whnf x == x" $ whnf x `shouldBe` x

    let f = Abs "x" $ App (Var "x") (Var "y")
     in do
          it "hnf (\\x -> x y) == \\x -> x y" $ hnf f `shouldBe` f
          it "whnf (\\x -> x y) == \\x -> x y" $ whnf f `shouldBe` f

    let t = App (Var "x") (Var "y")
     in do
          it "hnf (x y) == x y" $ hnf t `shouldBe` t
          it "whnf (x y) == x y" $ whnf t `shouldBe` t

    let f = Var "f"
        g = Abs "x" $ App (Var "f") (Var "x")
        y = Var "y"
        t = App g y
     in do
          it "hnf ((\\x -> f x) y) == f y" $ hnf t `shouldBe` App f y
          it "whnf ((\\x -> f x) y) == f y" $ whnf t `shouldBe` App f y
