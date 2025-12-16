module LinAlg where

import Gates
import Numeric.LinearAlgebra

type CMat = Matrix (Complex Double)
data Qubit = Qubit (Complex Double) (Complex Double)
  deriving (Show)

instance Num Qubit where
  (Qubit a b) + (Qubit c d) = Qubit (a+b) (c+d)
  (Qubit a b) - (Qubit c d) = Qubit (a-b) (c-d)
  (Qubit a b) * (Qubit c d) = Qubit (a*b) (c*d)
  fromInteger n = Qubit (fromInteger n) 0
  abs (Qubit a b) = Qubit (abs a) (abs b)
  signum (Qubit a b) = Qubit (signum a) (signum b)


ii :: Complex Double
ii = 0 :+ 1

toVector :: Qubit -> Vector (Complex Double)
toVector (Qubit a b) = 2 |> [a, b]

toQubit :: Vector (Complex Double) -> Qubit
toQubit v = Qubit (v ! 0) (v ! 1)

evalSingle  :: SingleGate -> Qubit -> Qubit
evalSingle gate qb = toQubit $ mat #> toVector qb
  where
    mat = case gate of
      I -> (2 >< 2) [1,0,
                    0,1]

      X -> (2 >< 2) [0,1,
                    1,0]

      Y -> (2 >< 2) [-ii, 0,
                      0, ii]

      Z -> (2 >< 2) [1,0,
                    0,-1]

      H -> let s = 1/sqrt 2
          in  s * (2><2) [1, 1,
                          1,-1]