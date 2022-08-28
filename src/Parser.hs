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

module Parser (Name (..), Term (..), Statement (..), program) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (foldr')
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Unicode.Char.Identifiers (isPatternWhitespace, isXIDContinue, isXIDStart)

import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO: StateT TopLevelDefs (InputT IO) for REPL and use haskeline.

{- Grammar Notes
  * Application is parsed with the highest precedence and associativity to the left, i.e.
    f x y = ((f x) y).
  * Abstractions are parsed with bodies extending as far to the right as possible, i.e.
    λx -> λy -> x y x = (λx -> (λy -> ((x y) x))).
  * Bracketing of terms can be used to override precedence and associativity when
    required.
  * Statements are either an assignment (let X := Y) or a λ-calculus term and end with a
    full stop '.'.

  For anyone editing this file: One good test to flush out any bugs in the parser is to
  check whether the following equality is reflected by the parse trees of the LHS and RHS
  respectively:
  λf -> (λx -> f (x x)) (λx -> f (x x)) = (λf -> ((λx -> (f (x x))) (λx -> (f (x x))))).
-}

{- | The parser monad.

 This is a Parsec type synonym to both help type inference and the compiler's
 optimiser.
-}
type Parser = Parsec Void Text

{- | A name in a program.

 A name is an identifier that is either used as a variable or an identifier to which an
 arbitrary term is bound.
-}
newtype Name = Name Text deriving (Eq, Show)

-- | A program statement.
data Statement
  = -- | A λ-term.
    Term Term
  | -- | An assignment of a name to a λ-term.
    Ass Name Term
  deriving (Eq, Show)

-- | A λ-term.
data Term
  = -- | A variable.
    Var Name
  | -- | A λ-abstraction.
    Abs Name Term
  | -- | An application of a λ-abstraction.
    App Term Term
  deriving (Eq, Show)

-- | Parse and discard one or more whitespace characters.
space1 :: Parser ()
space1 = void $ some (satisfy isPatternWhitespace)

-- | Parse and discard zero or more whitespace characters and comments.
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "/-" "-/")

lexeme :: Parser Text -> Parser Text
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Create a parser that will parse and discard the given string.
punc :: Text -> Parser ()
punc = void . symbol

{- | Create a parser that applies the given parser to an expression between a pair of
     round brackets.
-}
brackets :: Parser a -> Parser a
brackets = between (punc "(") (punc ")")

-- | Parse a 'start' character in a name.
nameStart :: Parser Char
nameStart = satisfy \c -> c /= 'λ' && isXIDStart c

-- | Parse a sequence of 'continue' characters in a name.
nameContinue :: Parser [Char]
nameContinue = many $ satisfy \c -> isXIDContinue c || c == '\''

-- | Parse a name according to the Unicode Standard Annex #31.
name :: Parser Name
name =
  Name <$> lexeme do
    start <- nameStart
    continue <- nameContinue
    return $ T.pack (start : continue)

-- | Parser for a variable term.
variable :: Parser Term
variable = Var <$> name <?> "variable"

-- | Parser for a λ-abstraction.
abstraction :: Parser Term
abstraction = do
  punc "\\" <|> punc "λ"
  head' <- (name <?> "variable") `sepBy1` spaceConsumer
  punc "->" <|> punc "↦"
  body <- term

  return $ desugar head' body
 where
  -- Expand an abstraction with multiple variables into its internal representation of
  -- nested single-variable abstractions.
  desugar = flip $ foldr' Abs

-- | Parser for an application term.
application :: Parser (Term -> Term -> Term)
application = return App

-- | Parser for a term.
term :: Parser Term
term = makeExprParser (choice indivisibles) operatorTable
 where
  indivisibles = [variable, abstraction, brackets term]
  operatorTable :: [[Operator Parser Term]]
  operatorTable = [[InfixL application]]

-- | Parser for an assignment statement.
assignment :: Parser Statement
assignment = do
  punc "let"
  name' <- name <?> "name"
  punc ":="
  term' <- term

  return $ Ass name' term'

-- | Parser for a term statement.
termS :: Parser Statement
termS = Term <$> term

-- | Parser for a program statement.
statement :: Parser Statement
statement = assignment <|> termS

-- | Parser for the end of a statement.
eos :: Parser ()
eos = punc "."

{- | Parser for a complete program file.

  We define 'program' in this context to be a sequence of bindings and/or terms.
-}
program :: Parser [Statement]
program = spaceConsumer *> statement `endBy` eos <* eof
