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
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  progName <- T.pack <$> getProgName
  args <- fmap T.pack <$> getArgs
  case args of
    [] -> TIO.putStrLn $ usage progName
    [filename] -> parseProgram filename
    _ -> TIO.putStrLn "Error: unexpected argument"

usage :: Text -> Text
usage progName = "Usage: " <> progName <> " [filename]"

-- TODO: Actually evaluate the program!
parseProgram :: Text -> IO ()
parseProgram filename = do
  let filename' = T.unpack filename
  -- NOTE: Program files are strictly considered to have a UTF-8 encoding.
  file <- decodeUtf8 <$> BS.readFile filename'

  case parse program filename' file of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right ast -> mapM_ print ast
