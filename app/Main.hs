module Main where

import Lib
import Parser
import Text.Parsec.String
import Html
import System.Environment

main :: IO ()
main = do
  (filename:outputFilename:_) <- getArgs
  result <- parseFromFile parserMarkdown filename 
  case result of
    Left err -> print err
    Right markdown -> let html = genHtml markdown
                      in  do
                        writeFile outputFilename html
                        putStrLn "Done!"
