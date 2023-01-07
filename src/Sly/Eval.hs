-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Sly.Eval (
  Bindings,
  Program (..),
  mkProgram,
  fileToProgram,
  stringToProgram,
  runProgram,
  hnf,
  whnf,
  alpha,
  subst,
  freeVariables,
  boundVariables,
) where

import Data.Foldable (foldr')
import Data.Sequence (Seq)
import Data.Set (Set, (\\))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Sly.Parser

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Type alias for a map of top-level let bindings.
type Bindings = Seq (Name, Term)

-- | A sly program.
data Program = Program {bindings :: Bindings, terms :: [Term]}

-- | Create a program from a list of statements.
mkProgram :: [Statement] -> Program
mkProgram statements =
  Program{bindings = extractBindings statements, terms = extractTerms statements}
 where
  extractBindings =
    Seq.fromList
      . map (\case (Ass n t) -> (n, t); _ -> error "Unreachable!")
      . filter (\case Term _ -> False; Ass _ _ -> True)
  extractTerms =
    map (\case (Term t) -> t; _ -> error "Unreachable!")
      . filter (\case Term _ -> True; Ass _ _ -> False)

-- | Load a program from a file.
fileToProgram :: FilePath -> IO (Either (ParseErrorBundle Text Void) Program)
fileToProgram path = do
  -- NOTE: Program files are strictly considered to have a UTF-8 encoding.
  program <- decodeUtf8 <$> BS.readFile path
  return case parse path program of
    Left bundle -> Left bundle
    Right statements -> Right (mkProgram statements)

-- | Load a program from a string.
stringToProgram :: Text -> Either (ParseErrorBundle Text Void) Program
stringToProgram s =
  case parse "<anonymous>" s of
    Left bundle -> Left bundle
    Right statements -> Right (mkProgram statements)

-- | Run a program.
runProgram :: Program -> [Term]
runProgram program = map (hnf . applyBindings program.bindings) program.terms
 where
  -- TODO: Implement bindings without this caveat!
  -- NOTE: The use of foldr is to ensure that bindings are applied in a way that allows
  --       them to depend on earlier bindings defined in an interactive session or file.
  applyBindings = flip $ foldr' \(k, v) t -> App (Abs k t) v

-- | Reduce a term to head normal form.
hnf :: Term -> Term
hnf t = case whnf t of
  Var v -> Var v
  Abs v m -> Abs v (hnf m)
  App m n -> App (hnf m) (hnf n)

-- | Reduce a term to weak head normal form.
whnf :: Term -> Term
whnf (App s t) | Abs x body <- whnf s = whnf $ subst x t body
whnf t = t

-- | Perform α-conversion on a term, if possible.
alpha :: Name -> Name -> Term -> Term
alpha x y t = case t of
  Var _ -> t
  Abs z body
    | x == z && y `notFreeIn` body -> Abs y (subst x (Var y) body)
    | otherwise -> Abs z (alpha x y body)
  App t1 t2 -> App (alpha x y t1) (alpha x y t2)

-- | Replace all free occurences of the variable x in a term t with the term
-- s.
subst :: Name -> Term -> Term -> Term
subst x s = go
 where
  go v@(Var y)
    | x == y = s
    | otherwise = v
  go f@(Abs y body)
    | x == y = f
    | y `Set.notMember` fvs = Abs y (go body)
    | otherwise =
      -- At this point, we must avoid variable capture. So...
      let z = chooseName y (Set.singleton y <> freeVariables body) -- pick a new name,
          u = alpha y z f -- α-convert,
       in go u -- and continue!
  go (App t1 t2) = App (go t1) (go t2)

  fvs = freeVariables s
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
