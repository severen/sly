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

module Eval (
  Context (..),
  emptyContext,
  mkContext,
  fileToContext,
  stringToContext,
  alpha,
  beta,
  subst,
  freeVariables,
  boundVariables,
) where

import Data.Map.Strict (Map)
import Data.Set (Set, (\\))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Parser

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | An evaluation context for a sly program.
data Context = Context {bindings :: Map Name Term, terms :: [Term]}
  deriving (Show, Eq)

-- | An empty evaluation context.
emptyContext :: Context
emptyContext = Context{bindings = Map.empty, terms = []}

-- | Create an evaluation context from a list of statements.
mkContext :: [Statement] -> Context
mkContext statements =
  Context{bindings = extractBindings statements, terms = extractTerms statements}
 where
  extractBindings =
    Map.fromList
      . map (\case (Ass n t) -> (n, t); _ -> error "Unreachable!")
      . filter (\case Term _ -> False; Ass _ _ -> True)
  extractTerms =
    map (\case (Term t) -> t; _ -> error "Unreachable!")
      . filter (\case Term _ -> True; Ass _ _ -> False)

-- | Create an evaluation context from a program file.
fileToContext :: FilePath -> IO (Either (ParseErrorBundle Text Void) Context)
fileToContext path = do
  -- NOTE: Program files are strictly considered to have a UTF-8 encoding.
  program <- decodeUtf8 <$> BS.readFile path
  return case parse path program of
    Left bundle -> Left bundle
    Right statements -> Right (mkContext statements)

-- | Create an evaluation context from a program string.
stringToContext :: Text -> Either (ParseErrorBundle Text Void) Context
stringToContext program =
  case parse "<anonymous>" program of
    Left bundle -> Left bundle
    Right statements -> Right (mkContext statements)

-- | Perform one step of β-reduction on a term, if possible.
beta :: Term -> Term
beta (App (Abs x body) t) = subst x t body
beta t = t

-- | Perform α-conversion on a term, if possible.
alpha :: Name -> Name -> Term -> Term
alpha x y t = case t of
  Var _ -> t
  Abs z body
    | x == z && y `notFreeIn` body -> Abs y (subst x (Var y) body)
    | otherwise -> Abs z (alpha x y body)
  App t1 t2 -> App (alpha x y t1) (alpha x y t2)

-- | Substitute all occurences of x in t with s.
subst :: Name -> Term -> Term -> Term
subst x s t = case t of
  Var y
    | x == y -> s
    | otherwise -> t
  Abs y body
    | x == y -> t
    | y `notFreeIn` s -> Abs y (subst x s body)
    | otherwise ->
      -- At this point, we must avoid variable capture. So...
      let z = chooseName y (Set.singleton y <> freeVariables body) -- pick a new name,
          u = alpha y z t -- α-convert,
       in subst x s u -- and try again!
  App t1 t2 -> App (subst x s t1) (subst x s t2)
 where
  chooseName :: Name -> Set Name -> Name
  chooseName (Name n) ys =
    head $ filter (`Set.notMember` ys) $ Name . (n <>) <$> iterate (<> "'") T.empty

-- | Determine whether a variable is free in a given term.
freeIn :: Name -> Term -> Bool
freeIn x t = x `Set.member` freeVariables t

-- | Determine whether a variable is not free in a given term.
notFreeIn :: Name -> Term -> Bool
notFreeIn x t = not (freeIn x t)

-- | Calculate the set of free variables in a term.
freeVariables :: Term -> Set Name
freeVariables (Var x) = Set.singleton x
freeVariables (Abs x body) = freeVariables body \\ Set.singleton x
freeVariables (App s t) = freeVariables s <> freeVariables t

-- | Calculate the set of bound variables in a term.
boundVariables :: Term -> Set Name
boundVariables (Var _) = Set.empty
boundVariables (Abs x body) = Set.singleton x <> boundVariables body
boundVariables (App s t) = boundVariables s <> boundVariables t
