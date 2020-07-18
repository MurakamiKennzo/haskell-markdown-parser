module Main where

import Lib
import Parser
import Text.Parsec

main :: IO ()
main = do
  print $ parse (parseInlines) "Unknown" "[i13](https://muimage)rt***fgg***455*6***77**"
