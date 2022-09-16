module PropertyTests (propertyTests) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Sly.Parser (fromChurch, toChurch)

propertyTests :: TestTree
propertyTests =
  fromGroup $
    Group
      "Property Tests"
      [ ("forall n. (fromChurch . toChurch) n == n", prop_fromChurch_toChurch)
      ]

prop_fromChurch_toChurch :: Property
prop_fromChurch_toChurch = property $ do
  -- fromChurch is _almost_ a left-identity for toChurch.
  n <- forAll $ Gen.integral (Range.linear 0 1000)
  (fromChurch . toChurch) n === Just n
