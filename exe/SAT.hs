module Main where

import Parser
import Grovers
import Generator
import Eval
import Gates
import Measure

main :: IO ()
main = 
  let 
      solutions = 1
      width = 4

      oracle = [MCZ [0, 1], MCZ [1, 2], MCZ [1, 2, 3]]
      groversCircuit = grovers oracle width

      -- ???
      -- h = evalProgram (pow H width) (zero width)
      result = evalByParts 5 groversCircuit (zero width)
      -- profit
   in print . vectorize =<< result
