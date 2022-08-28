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

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Environment
import Text.Megaparsec (errorBundlePretty, parse)

import Parser

import qualified Data.ByteString as BS
import qualified Data.Text as T

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["--help"] -> putStrLn (help progName)
    [filename] -> parseProgram filename
    _ -> putStrLn "Error: unexpected argument"

-- | Given the program name, format a help string.
help :: String -> String
help progName =
  "Usage: " <> progName <> " [options] [filename]\n\n"
    <> "Interpreter for the pure untyped λ-calculus.\n\n"
    <> "Options:\n"
    <> "--help  Show this message and exit."

-- TODO: Actually evaluate the program!
parseProgram :: Text -> IO ()
parseProgram filename = do
  let filename' = T.unpack filename
  -- NOTE: Program files are strictly considered to have a UTF-8 encoding.
  file <- decodeUtf8 <$> BS.readFile filename'

  case parse program filename' file of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right ast -> mapM_ print ast
