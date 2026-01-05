module ANF where

import AST

data ANF
  = Cst Bool
  | Pos Int
  | Xor ANF ANF
  | And ANF ANF
  deriving (Eq, Show)

astToAnf :: Exp -> ANF
astToAnf (Const b) = Cst b
astToAnf (Var i) = Pos i
astToAnf (NEG e) = Xor (astToAnf e) (Cst True)
astToAnf (XOR e1 e2) = Xor (astToAnf e1) (astToAnf e2)
astToAnf (OR e1 e2) =
  let e1' = astToAnf e1
      e2' = astToAnf e2
   in Xor (Xor e1' e2') (And e1' e2') -- equivalent to elimORwXOR
astToAnf (AND e1 e2) = And (astToAnf e1) (astToAnf e2)

-- distribute And across Xor.
distributeAnd :: ANF -> ANF
distributeAnd anf =
  case anf of
    And e1 e2 -> distAnd (dist e1) (dist e2)
    Xor e1 e2 -> Xor (dist e1) (dist e2)
    _ -> anf
  where
    dist a = distributeAnd a
    distAnd e1 e2 =
      case (e1,e2) of
        (Xor a b,c) -> Xor (distAnd a c) (distAnd b c)
        (a,Xor b c) -> Xor (distAnd a b) (distAnd a c)
        _ -> And e1 e2
