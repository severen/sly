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

module Eval (Context (..), emptyContext, mkContext, fileToContext, stringToContext) where

import Data.Map.Strict (Map, (!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Parser

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

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
