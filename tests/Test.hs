-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: CC0-1.0

module Main where

import Test.Tasty

import PropertyTests
import UnitTests

main :: IO ()
main = do
  tree <- tests
  defaultMain tree

tests :: IO TestTree
tests = do
  unitTests' <- unitTests
  return $ testGroup "Tests" [unitTests', propertyTests]
