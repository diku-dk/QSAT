module Grovers where

import AST
import Quantumize
import HQP.QOp.Syntax

prepareOracle :: Int -> QOp
prepareOracle 0 = One
prepareOracle n = prepareOracle (n-1) <.> I <> cx n 0 n

mcz :: Int -> QOp
mcz 0 = One
mcz 1 = Z
mcz n = C $ mcz $ n-1

diffusion :: CircuitWidth -> QOp
diffusion width = pow H width <> pow X width <> mcz width <> pow X width <> pow H width

grovers' :: Int -> CircuitWidth -> Program -> Program
grovers' 0 _ _ = []
grovers' n width oracle = oracle ++ [Unitary $ prepareOracle width, Unitary $ diffusion width] ++ grovers' (n-1) width oracle

grovers :: Int -> CircuitWidth -> Program -> Program
grovers n width oracle = Unitary (pow H width) : grovers' n width oracle