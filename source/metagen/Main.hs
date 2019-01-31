module Main where

import System.IO

import Parser
import Text.Megaparsec

main :: IO ()
main = do
  handle   <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  --parseTest (quoted <* eof) contents
  hClose handle
