-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: CC0-1.0

module PropertyTests (propertyTests) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Sly.Syntax (fromChurchNat, toChurchNat, fromChurchBool, toChurchBool)

propertyTests :: TestTree
propertyTests =
  fromGroup $
    Group
      "Property Tests"
      [ ("forall n. (fromChurchNat . toChurchNat) n == n", prop_fromChurchNat_toChurchNat)
      , ("forall b. (fromChurchBool . toChurchBool) b == b", prop_fromChurchBool_toChurchBool)
      ]

prop_fromChurchNat_toChurchNat :: Property
prop_fromChurchNat_toChurchNat = property do
  -- fromChurchNat is _almost_ a left-identity for toChurchNat.
  n <- forAll $ Gen.integral (Range.linear 0 1000)
  (fromChurchNat . toChurchNat) n === Just n

prop_fromChurchBool_toChurchBool :: Property
prop_fromChurchBool_toChurchBool = property do
  -- fromChurchBool is _almost_ a left-identity for toChurchBool.
  b <- forAll Gen.bool
  (fromChurchBool . toChurchBool) b === Just b
