{-
  sly — An interpreter for the pure untyped λ-calculus.
  Copyright © 2022 Severen Redwood

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
-}

module Syntax (
  Name (..),
  Statement (..),
  Term (..),
  astShow,
  toChurch,
  fromChurch
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

    -- | Slurp up λ-abstractions!
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
