module Gates where

-- the building bricks of quantum simulation.
type QubitPos = Int
type AncillaPos = Int

type CircuitWidth = Int

data SingleGate
  = X | H
  deriving(Show)

data Gate 
  = Only QubitPos SingleGate
  | MCZ [QubitPos]
  | MCX [QubitPos] QubitPos
  deriving(Show)

type Program = [Gate]

pow :: SingleGate -> Int -> Program
pow gate n = map (`Only` gate) [0..n-1]

ancilla2absolute :: CircuitWidth -> AncillaPos -> QubitPos
ancilla2absolute width ancillapos = width + ancillapos