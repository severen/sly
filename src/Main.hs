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

module Main (main) where

import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline
import System.Environment
import Text.Megaparsec (errorBundlePretty)

import Eval

import qualified Data.Text as T

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> repl
    ["--help"] -> putStrLn (help progName)
    [path] -> do
      result <- fileToContext path
      case result of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right context -> mapM_ print context.terms
    _ -> putStrLn "Error: unexpected argument"

help :: String -> String
help progName =
  "Usage: " <> progName <> " [options] [filename]\n\n"
    <> "Interpreter for the pure untyped λ-calculus.\n\n"
    <> "Options:\n"
    <> "--help  Show this message and exit."

repl :: IO ()
repl = do
  putStrLn "Welcome to sly v0.1.0!\nType :quit or press C-d to exit."
  evalStateT (runInputT defaultSettings loop) emptyContext
 where
  loop :: InputT (StateT Context IO) ()
  loop = do
    minput <- getInputLine "~> "
    case minput of
      Nothing -> return ()
      Just input
        | input == ":q" || input == ":quit" -> return ()
        | otherwise -> do
          context <- lift get
          case stringToContext (T.pack input) of
            Left bundle -> outputStr (errorBundlePretty bundle)
            Right lineContext -> do
              lift $ put context{terms = lineContext.terms}
              mapM_ (outputStrLn . show) lineContext.terms
          loop
