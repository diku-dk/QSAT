module Tests where

import ANF ( PolySystem, ps2width, ps2oracle, simplifyPs, maxVar )
import Verif
import Test.QuickCheck
import Eval (evalProgram, zero, scanProgram, hadamard, initial)
import Measure (vectorize, seperateSolution, removeAncilla)
import Grovers (grovers)
import Gates (CircuitWidth, CircuitDescriptor)
import Data.List (nub)
import Data.Vector.Storable (Vector)

-- genBitStrings :: Int -> Gen BitString
-- genBitStrings n = vectorOf n arbitrary

groverCheatTest :: PolySystem -> Property
groverCheatTest ps =
  property $
    let 
      width = ps2width ps
      (solSet1, solSet2) = runGroverCheat width ps 
    in
      case (nub $ verifPS ps <$> solSet1, nub $ verifPS ps <$> solSet2) of
        ([truthVal1], [truthVal2]) -> truthVal1 /= truthVal2
        ([_], _) -> True
        (_, [_]) -> True
        _ -> False

runGroverCheat :: CircuitDescriptor -> PolySystem -> ([BitString], [BitString])
runGroverCheat width ps =
  let oracle = ps2oracle ps
      groversCircuit = grovers width oracle 1
      stateVector = vectorize $ evalProgram groversCircuit $ initial width
  in (pairMap . map) (padded2bin (fst width)) (seperateSolution (removeAncilla width stateVector))

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

-- runGrover :: CircuitWidth -> PolySystem -> Maybe (Vector Double)
-- runGrover width e = do
--   oracle <- anf2oracle <$> exp2anf e
--   let groversCircuit = grovers width oracle 1
--   let solution = vectorize $ evalProgram groversCircuit (zero width)
--   pure solution