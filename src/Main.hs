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

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, modify)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf, stripPrefix)
import System.Console.Haskeline
import System.Environment
import System.Random (initStdGen, uniformR)
import Text.Megaparsec (errorBundlePretty)

import Syntax (Name (..), astShow)
import Eval

import qualified Data.Text as T

-- NOTE: This should not conflict with valid program syntax.
-- | The prefix used for REPL commands.
commandPrefix :: String
commandPrefix = ":"

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> repl
    ["--help"] -> putStrLn (help progName)
    [path] -> do
      result <- fileToProgram path
      case result of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right program -> mapM_ print (runProgram program)
    _ -> putStrLn "Error: unexpected argument"

help :: String -> String
help progName =
  "Usage: " <> progName <> " [options] [filename]\n\n"
    <> "Interpreter for the pure untyped λ-calculus.\n\n"
    <> "Options:\n"
    <> "--help  Show this message and exit."

repl :: IO ()
repl = do
  let adjectives = ["cunning", "crafty", "guileful", "shrewd"]

  stdGen <- liftIO initStdGen
  let (n, _) = uniformR (0, length adjectives - 1) stdGen
  putStrLn $
    "Welcome to sly v0.1.0, the " <> adjectives !! n <> " λ-calculus interpreter!\n"
      <> "Type :quit or press C-d to exit."

  evalStateT (runInputT defaultSettings loop) mempty
 where
  loop :: InputT (StateT Bindings IO) ()
  loop = do
    minput <- getInputLine "~> "
    case minput of
      Nothing -> return ()
      Just input
        | commandPrefix `isPrefixOf` input -> do
          shouldQuit <- runCommand (tail input)
          unless shouldQuit loop
        | otherwise -> do
          case stringToProgram (T.pack input) of
            Left bundle -> outputStr (errorBundlePretty bundle)
            Right program -> do
              lift $ modify (<> program.bindings)
              bindings <- lift get
              mapM_ (outputStrLn . show) $
                runProgram Program{bindings, terms = program.terms}
          loop

-- TODO: Refactor this to be less ad-hoc.
-- | Run an interpreter command.
runCommand :: String -> InputT (StateT Bindings IO) Bool
runCommand input
  | command `elem` ["q", "quit"] = return True
  | command == "parse", Just term <- stripPrefix "parse " input = do
    case stringToProgram (T.pack term) of
      Left bundle -> outputStr (errorBundlePretty bundle)
      Right program -> mapM_ (outputStrLn . astShow) program.terms
    return False
  | command == "load", Just filepath <- stripPrefix "load " input = do
    result <- liftIO $ fileToProgram filepath
    case result of
      Left bundle -> outputStr (errorBundlePretty bundle)
      Right program -> do
        mapM_ (outputStrLn . show) (runProgram program)
        lift $ modify (<> program.bindings)
    return False
  | command `elem` ["b", "bindings"] = do
    let format (Name n, t) = T.unpack n <> " := " <> show t
    bindings <- lift get
    mapM_ (outputStrLn . format) bindings
    return False
  | command == "clear" = lift $ put mempty >> return False
  -- TODO: Generate this help string more robustly.
  | command `elem` ["?", "h", "help"] = do
    outputStrLn $
      "Commands available from the prompt:\n"
        <> "  :parse <term>     show the parse tree for <term> in bracketed form\n"
        <> "  :load, :l <path>  load a sly program, evaluating its terms and adding\n"
        <> "                    its bindings to the environment\n"
        <> "  :bindings, :b     show all bindings in the environment\n"
        <> "  :clear            clear the environment\n"
        <> "  :help, :?         view this list of commands\n"
        <> "  :quit, :q         exit sly"
    return False
  | otherwise = do
    outputStrLn $ "Unrecognised REPL command: " <> command
    outputStrLn "Use :? for help."
    return False
 where
  command = head $ words input
