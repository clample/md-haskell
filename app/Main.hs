module Main where

import System.IO
import System.Directory
import MarkdownParser

main :: IO ()
main = do
  handle <- openFile "src/sample-markdown.md" ReadMode
  contents <- hGetContents handle
  putStr "Good"
  hClose handle
