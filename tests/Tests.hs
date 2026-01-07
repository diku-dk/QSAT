module Tests where

import AST
import ANF
import Verif
import Test.QuickCheck

maxVar :: Exp -> Int
maxVar (Atom (Var n)) = n
maxVar (Atom (Cst _)) = -1
maxVar (AND a b) = max (maxVar a) (maxVar b)
maxVar (XOR a b) = max (maxVar a) (maxVar b)
maxVar (OR a b) = max (maxVar a) (maxVar b)
maxVar (NEG a) = maxVar a

genBitStrings :: Int -> Gen BitString
genBitStrings n = vectorOf n arbitrary

exp2anfTest :: Exp -> Property
exp2anfTest e =
  forAll (genBitStrings (maxVar e + 1)) $
    \bs -> case exp2anf e of
      Nothing -> False
      Just anf -> verif e bs == verifANF anf bs