module Main where 

import AST
import Parser

main :: IO()
main = 
    let exp = NEG (Atom "p") in
    putStr "Hello"
