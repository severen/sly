-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Sly.Parser (
  Name (..),
  Term (..),
  Statement (..),
  parse,
  toChurch,
  fromChurch,
) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (foldr')
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (Token, parse)
import Unicode.Char.Identifiers (isPatternWhitespace, isXIDContinue, isXIDStart)

import Sly.Syntax

import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

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

-- | Words that are reserved as keywords and thus disallowed as names.
keywords :: [Text]
keywords = ["let", "in"]

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

lexeme :: Parser a -> Parser a
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
nameStart = satisfy \c -> isXIDStart c && c /= 'λ'

-- | Parse a sequence of 'continue' characters in a name.
nameContinue :: Parser [Char]
nameContinue = many $ satisfy \c -> isXIDContinue c || c == '\''

-- | Parse a name according to the Unicode Standard Annex #31.
name :: Parser Name
name = Name <$> (lexeme . try) (p >>= check)
 where
  p = T.pack <$> ((:) <$> nameStart <*> nameContinue)
  check s
    | s `notElem` keywords = return s
    -- TODO: See if the positioning of this error message (when output) can be improved.
    | otherwise = fail $ "keyword " <> T.unpack s <> " cannot be a name"

-- | Parse a variable term.
variable :: Parser Term
variable = Var <$> name <?> "variable"

-- | Parse a natural number.
natural :: Parser Term
natural = toChurch <$> (lexeme L.decimal >>= check)
 where
   check n
     | n <= toInteger maxInt = return (fromInteger n)
     | otherwise = fail $ "naturals larger than " <> show maxInt <> " are disallowed"
   maxInt = maxBound @Int

-- | Parse a λ-abstraction.
abstraction :: Parser Term
abstraction = do
  punc "\\" <|> punc "λ"
  binders <- (name <?> "variable") `sepBy1` spaceConsumer
  punc "->" <|> punc "↦"
  body <- term

  return $ abstract binders body
 where
  -- Expand an abstraction with multiple variables into its internal representation of
  -- nested single-variable abstractions.
  abstract = flip $ foldr' Abs

-- | Parse an application term.
application :: Parser (Term -> Term -> Term)
application = return App

-- | Parse the initial common fragment of a let term or a let statement.
lettStart :: Parser (Name, Term)
lettStart = do
  punc "let"
  name' <- name <?> "name"
  punc ":="
  term' <- term
  return (name', term')

-- | Parse a let term.
lett :: Parser Term
lett = do
  (x, t) <- lettStart
  punc "in"
  body <- term
  -- NOTE: let x := t in body is syntactic sugar for (λx -> body) t.
  return $ App (Abs x body) t

-- | Parse a λ-term.
term :: Parser Term
term = makeExprParser (choice indivisibles) operatorTable
 where
  indivisibles = [variable, natural, abstraction, lett, brackets term]
  operatorTable :: [[Operator Parser Term]]
  operatorTable = [[InfixL application]]

-- | Parse an assignment statement.
assignment :: Parser Statement
assignment = try do
  (x, t) <- lettStart
  notFollowedBy "in"
  return $ Ass x t

-- | Parse a term statement.
termS :: Parser Statement
termS = Term <$> term

-- | Parse a program statement.
statement :: Parser Statement
statement = assignment <|> termS

-- | Parse the end of a statement.
eos :: Parser ()
eos = punc "."

{- | Parse a complete program file.

  We define 'program' in this context to be a sequence of bindings and/or terms.
-}
program :: Parser [Statement]
program = spaceConsumer *> statement `endBy` eos <* eof

-- | Parse a sly program given a filename and a program string.
parse :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Statement]
parse = runParser program
