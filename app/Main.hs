module Main where

import Lib
import Parser
import Text.Parsec ( parse )

main :: IO ()
main = do
  print $ parse parseLink "Unknown" "[i13](https://muimage)"
