-- SPDX-FileCopyrightText: 2022 Severen Redwood <me@severen.dev>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Main (main) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf, stripPrefix)
import Effectful (Eff, IOE, MonadIO, liftIO, runEff, (:>))
import Effectful.State.Static.Local (State, evalState, get, modify, put)
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  runInputT,
 )
import System.Environment (getArgs, getProgName)
import System.Random (initStdGen, uniformR)
import Text.Megaparsec (errorBundlePretty)

import Sly.Eval (
  Bindings,
  Program (Program, bindings, terms),
  fileToProgram,
  runProgram,
  stringToProgram,
 )
import Sly.Syntax (Name (..), astShow)

import qualified Data.Text as T

-- NOTE: This should not conflict with valid program syntax.
-- | The prefix used for REPL commands.
commandPrefix :: String
commandPrefix = ":"

outputStr :: MonadIO m => String -> m ()
outputStr = liftIO . putStr

outputStrLn :: MonadIO m => String -> m ()
outputStrLn = liftIO . putStrLn

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> runEff repl
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

repl :: IOE :> es => Eff es ()
repl = do
  let adjectives = ["cunning", "crafty", "guileful", "shrewd"]

  stdGen <- liftIO initStdGen
  let (n, _) = uniformR (0, length adjectives - 1) stdGen
  outputStrLn $
    "Welcome to sly v0.1.0, the " <> adjectives !! n <> " λ-calculus interpreter!\n"
      <> "Type :quit or press C-d to exit."

  evalState mempty $ runInputT defaultSettings loop
 where
  loop :: (State Bindings :> es, IOE :> es) => InputT (Eff es) ()
  loop = do
    getInputLine "~> " >>= \case
      Nothing -> return ()
      Just input
        | commandPrefix `isPrefixOf` input -> do
          shouldQuit <- lift $ runCommand (tail input)
          unless shouldQuit loop
        | otherwise -> do
          case stringToProgram (T.pack input) of
            Left bundle -> outputStr (errorBundlePretty bundle)
            Right program -> do
              lift $ modify (<> program.bindings)
              bindings <- lift get
              mapM_ (liftIO . print) $
                runProgram Program{bindings, terms = program.terms}
          loop

-- TODO: Refactor this to be less ad-hoc.
-- | Run an interpreter command.
runCommand :: (State Bindings :> es, IOE :> es) => String -> Eff es Bool
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
        mapM_ (liftIO . print) (runProgram program)
        modify (<> program.bindings)
    return False
  | command `elem` ["b", "bindings"] = do
    let format (Name n, t) = T.unpack n <> " := " <> show t
    bindings <- get
    mapM_ (outputStrLn . format) bindings
    return False
  | command == "clear" = put mempty >> return False
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
