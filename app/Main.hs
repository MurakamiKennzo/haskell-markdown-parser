module Main where

import Lib
import Parser
import Text.Parsec

main :: IO ()
main = do
  print $ parse parserMarkdown "Unknown" "# hello world\nyou know me\n1. hello world\n2. world \n3. 33"
