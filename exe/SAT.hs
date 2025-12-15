module Main where 

import AST
import Parser
import Quantumize
import Grovers

main :: IO()
main = 
  let example = "p & q | (123 & ~123) & ( (x ^ y) ^ z)"
    in case parse example of
          Right x  -> putStrLn $ show x
          Left err -> putStrLn err
