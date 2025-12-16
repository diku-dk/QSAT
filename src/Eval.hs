module Eval where

import Gates
import LinAlg
import qualified Data.Vector as V

type PureTensor a = V.Vector a

type Tensor a = [PureTensor a]

zero :: Int -> Tensor Qubit
zero dim = [V.replicate dim (Qubit 0 0)]

evalProgram :: Program -> Tensor Qubit -> Tensor Qubit
evalProgram program tensor = foldl (flip evalGate) tensor program

evalGate :: Gate -> Tensor Qubit -> Tensor Qubit
evalGate gate = concatMap (evalTerm gate)

evalTerm :: Gate -> PureTensor Qubit -> Tensor Qubit
evalTerm (Only pos gate) qbs = pure $ qbs V.// [(pos, evalSingle gate (qbs V.! pos))]
evalTerm (Ctrl ctrls target gate) qbs = [qbs, correction]
  where
    dimension = length qbs
    beta = product [qbs V.! i | i <- ctrls]
    target' = evalSingle gate (qbs V.! target) - (qbs V.! target)
    correction = V.replicate dimension (Qubit 0 0) V.// ((target, target') : zip ctrls (repeat beta))

