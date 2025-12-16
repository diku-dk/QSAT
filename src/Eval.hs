module Eval(evalProgram) where

import Gates
import LinAlg
import qualified Data.Vector as V

type PureTensor a = V.Vector a

type Tensor a = [PureTensor a]

-- |0..0> in tensor notation
zero :: Int -> Tensor Qubit
zero dim = [V.replicate dim (qubit 1 0)]

-- apply all gates in a program sequentiall (via fold)
evalProgram :: Program -> Tensor Qubit -> Tensor Qubit
evalProgram program tensor = foldl (flip evalGate) tensor program

-- distributes a gate over addition to all pure tensors in a tensor
evalGate :: Gate -> Tensor Qubit -> Tensor Qubit
evalGate gate = concatMap (evalTerm gate)

-- evaluates a gate on a pure tensor, using LinAlg Module
evalTerm :: Gate -> PureTensor Qubit -> Tensor Qubit
evalTerm (Only pos gate) qbs = pure $ qbs V.// [(pos, evalSingle gate (qbs V.! pos))]
evalTerm (Ctrl ctrls target gate) qbs = [qbs, correction]
  where
    dimension = length qbs
    beta = product $ [qsnd (qbs V.! i) | i <- ctrls]
    targetVal' = beta *^ (evalSingle gate (qbs V.! target) - (qbs V.! target))
    correction = V.replicate dimension (qubit 0 0) V.// ((target, targetVal') : zip ctrls (repeat (qubit 0 beta)))

