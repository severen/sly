-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Sly.Syntax (
  Name (..),
  Statement (..),
  Term (..),
  astShow,
  toChurch,
  fromChurch,
) where

import Data.Text (Text)

import qualified Data.Text as T

{- | A name in a program.

 A name is an identifier that is either used as a variable or an identifier to which an
 arbitrary term is bound.
-}
newtype Name = Name Text deriving (Eq, Ord, Show)

-- | A program statement.
data Statement
  = -- | A λ-term.
    Term Term
  | -- | An assignment of a name to a λ-term.
    Ass {-# UNPACK #-} !Name Term
  deriving (Eq)

instance Show Statement where
  show (Term t) = show t <> "."
  show (Ass (Name n) t) = T.unpack $ "let " <> n <> " := " <> T.pack (show t) <> "."

-- | A λ-term.
data Term
  = -- | A variable.
    Var {-# UNPACK #-} !Name
  | -- | A λ-abstraction.
    Abs {-# UNPACK #-} !Name Term
  | -- | An application of a λ-abstraction.
    App Term Term
  deriving (Eq)

instance Show Term where
  show = T.unpack . go
   where
    go :: Term -> Text
    go (Var (Name n)) = n
    go (Abs (Name n) body) = "λ" <> n <> slurp body
    go (App l@(Abs _ _) r) = "(" <> go l <> ") " <> go r
    go (App l r@(App _ _)) = go l <> " (" <> go r <> ")"
    go (App l r) = go l <> " " <> go r

    -- Slurp up λ-abstractions!
    slurp :: Term -> Text
    slurp (Abs (Name n) body) = " " <> n <> slurp body
    slurp body = " -> " <> go body

-- | Like regular show, but displays the term fully bracketed.
astShow :: Term -> String
astShow = T.unpack . go
 where
  go (Var (Name n)) = n
  go (Abs (Name n) t) = "(λ" <> n <> " -> " <> go t <> ")"
  go (App l r) = "(" <> go l <> " " <> go r <> ")"

-- | Convert an integer into a Church numeral term.
toChurch :: Int -> Term
toChurch n =
  Abs (Name "f") $
    Abs (Name "x") $
      iterate (App (Var $ Name "f")) (Var $ Name "x") !! n

-- | Convert a term into an integer if it has the shape of a Church numeral.
fromChurch :: Term -> Maybe Int
fromChurch (Abs f (Abs x body)) = go 0 body
 where
  go :: Int -> Term -> Maybe Int
  go n = \case
    Var y | y == x -> Just n
    App (Var g) t | g == f -> go (n + 1) t
    _ -> Nothing
fromChurch _ = Nothing
