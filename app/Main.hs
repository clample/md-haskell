module Main where

import System.IO
import System.Directory
import System.Environment
import MarkdownParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do putStrLn "Please specify an input and output path"
    readPath:[] -> do putStrLn "Please specify an input and output path"
    readPath:writePath:[] -> do readHandle <- openFile readPath ReadMode
                                contents <- hGetContents readHandle
                                writeHandle <- openFile (writePath) WriteMode
                                hPutStr writeHandle (parseAndRenderHtml parseMarkdown contents)
                                hClose readHandle
                                hClose writeHandle
