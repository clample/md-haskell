module Main where

import System.IO
import System.Directory
import Lib

main :: IO ()
main = do
  handle <- openFile "src/sample-markdown.md" ReadMode
  contents <- hGetContents handle
  putStr $ parseMarkdown contents
  hClose handle
