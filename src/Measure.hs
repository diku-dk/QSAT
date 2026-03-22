module Measure where

import Eval ( Tensor, PureTensor(..), Qubit(..), (~=), evalProgram)
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.List (findIndices)
import Gates

--- measurement ---

unQubit :: Qubit -> Vector Double
unQubit Ket0 = fromList [1, 0]
unQubit Ket1 = fromList [0, 1]
unQubit KetPlus = fromList [sqrt 2 / 2, sqrt 2 / 2]
unQubit KetMinus = fromList [sqrt 2 / 2, - (sqrt 2 / 2)]

vectorize :: Tensor -> Vector Double
vectorize t = cmap roundZero $ sum $ vectorizePT <$> t

vectorizePT :: PureTensor -> Vector Double
vectorizePT (PT z v) = scale z $ V.foldl1 f (unQubit <$> V.reverse v)
  where f x y = flatten $ outer x y

roundZero :: Double -> Double
roundZero x = if x ~= 0 then 0 else x

greedyMeasure :: Vector Double -> Int
greedyMeasure v = VS.maxIndex $ VS.map (\x -> x*x) v

project :: CircuitDescriptor -> Vector Double -> Vector Double
project = undefined

--- cheaty inspection ---
removeAncilla :: CircuitDescriptor -> Vector Double -> Vector Double
 -- TODO: add check that ancillas are property zeroed
removeAncilla cd v = 
  let 
    in_space_size = 2 ^ fst cd
    (input_v, ancilla_v) = VS.splitAt in_space_size v
  in
    input_v

seperateSolution :: Vector Double -> ([Int], [Int])
seperateSolution v =
  let
    l = VS.toList v
    mu = sum l / fromIntegral (length l)
  in case findIndices (~= mu) l of
    [] -> (findIndices (>= mu) l, findIndices (< mu) l)
    s -> (s, [])

evalByPartMeasure :: Int -> Program -> Tensor -> IO Tensor
evalByPartMeasure _ [] t = pure t
evalByPartMeasure n prog t = do
    let prog1 = take n prog
    let t' = evalProgram prog1 t
    putStrLn $ show (length t') ++ "," ++ show (last prog1)
    print $ vectorize t'
    evalByPartMeasure n (drop n prog) t'