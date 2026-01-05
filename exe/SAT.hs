module Main where

import Comp
import Grovers
import Parser
import PhaseEval

main :: IO ()
main = 
  let 
      -- obtain input - substitute appropriate input stream later
      example = "(~p & q) | (t & ~p)"

      -- parse input
      (bexp,n) = parseWithUnique example

      -- create boolean (phase)oracle
      oracle = phaseOracle bexp

      -- apply Grover's algorithm
      fullGrover = grover oracle n

      -- ??? (evaluate grover on an empty state)
      result = evalProgram fullGrover n

      -- profit
   in putStrLn $ show result
