module Grovers where

import Gates

-- diffussion step (reflect across the equal superposition vector)
diffusion :: CircuitWidth -> Program
diffusion width = pow H width ++ pow X width ++ [MCZ [0..width-1]] ++ pow X width ++ pow H width

grovers :: CircuitDescriptor -> Program -> Int -> Program -- groversCuircuit that does infinite iterations, just take as much as you need
grovers (inputWidth, _ancillaWidth) oracle iterations = concat (replicate iterations (oracle ++ diffusion inputWidth))