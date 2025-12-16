module Main where 

import AST
import Parser
import Quantumize
import Grovers
import Generator

main :: IO()
main = putStrLn "Hello world"
  --putStrLn $ show $ gen 5 1000
  --let example = "p & q | (123 & ~123) & ( (x ^ y) ^ z)"
  --  in case parse example of
  --        Right x  -> putStrLn $ show x
  --       Left err -> putStrLn err
