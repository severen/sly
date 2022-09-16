module UnitTests (unitTests) where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Sly.Eval (hnf, whnf)
import Sly.Parser (fromChurch, toChurch)
import Sly.Syntax (Name (..), Term (..))

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" do
  describe "Church Numerals" $ do
    it "0 == \\f x -> x" $ do
      let zero = Abs (Name "f") (Abs (Name "x") (Var $ Name "x"))
      toChurch 0 `shouldBe` zero
      fromChurch zero `shouldBe` Just 0

    it "1 == \\f x -> f x" $ do
      let one = Abs (Name "f") (Abs (Name "x") $ App (Var $ Name "f") (Var $ Name "x"))
      toChurch 1 `shouldBe` one
      fromChurch one `shouldBe` Just 1

    it "2 == \\f x -> f (f x)" $ do
      let two =
            Abs (Name "f") $
              Abs (Name "x") $
                App (Var $ Name "f") (App (Var $ Name "f") (Var $ Name "x"))
      toChurch 2 `shouldBe` two
      fromChurch two `shouldBe` Just 2

  describe "Evaluator" $ do
    let x = (Var $ Name "x")
     in do
          it "hnf x == x" $ hnf x `shouldBe` x
          it "whnf x == x" $ whnf x `shouldBe` x

    let f = Abs (Name "x") (App (Var $ Name "x") (Var $ Name "y"))
     in do
          it "hnf (\\x -> x y) == \\x -> x y" $ hnf f `shouldBe` f
          it "whnf (\\x -> x y) == \\x -> x y" $ whnf f `shouldBe` f

    let t = App (Var $ Name "x") (Var $ Name "y")
     in do
          it "hnf (x y) == x y" $ hnf t `shouldBe` t
          it "whnf (x y) == x y" $ whnf t `shouldBe` t

    let f = Var $ Name "f"
        g = Abs (Name "x") (App (Var $ Name "f") (Var $ Name "x"))
        y = Var $ Name "y"
        t = App g y
     in do
          it "hnf ((\\x -> f x) y) == f y" $ hnf t `shouldBe` App f y
          it "whnf ((\\x -> f x) y) == f y" $ whnf t `shouldBe` App f y
