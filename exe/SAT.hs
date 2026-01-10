module Main where

-- import Parser
import Grovers
import Eval
import Gates
import Measure

main :: IO ()
main = 
  let 
      width = 5
      iterations = 10

      oracle = [MCZ[4, 0], MCZ [0, 1], MCZ [1, 2], MCZ [1, 2, 3], MCZ [1, 3]]
      groversCircuit = grovers width oracle iterations

      -- ???
      -- h = evalProgram (pow H width) (zero width)
      result = scanProgram groversCircuit (zero width)
      -- profit
   in print result
