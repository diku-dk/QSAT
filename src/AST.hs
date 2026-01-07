module AST where

import Test.QuickCheck

data Atom =
    Cst Bool
  | Var Int
  deriving(Eq)

instance Show Atom where
  show (Cst True) = "1"
  show (Cst False) = "0"
  show (Var n) = "x" ++ show n

instance Arbitrary Atom where
  arbitrary = sized genAtom
  shrink = shrinkAtom

data Exp = 
    Atom Atom
  | AND Exp Exp 
  | OR Exp Exp
  | XOR Exp Exp 
  | NEG Exp
  deriving (Eq)

instance Show Exp where
  show (Atom atom) = show atom
  show (AND a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (OR a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (XOR a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (NEG a) = "~" ++ show a

instance Arbitrary Exp where
  arbitrary = sized genExp
  shrink = shrinkExp

genExp :: Int -> Gen Exp
genExp n = oneof
  [ Atom <$> arbitrary
  , AND <$> genExp (n `div` 2) <*> genExp (n `div` 2)
  , OR <$> genExp (n `div` 2) <*> genExp (n `div` 2)
  , XOR <$> genExp (n `div` 2) <*> genExp (n `div` 2)
  , NEG <$> genExp (n-1)
  ]

genAtom :: Int -> Gen Atom
genAtom n = elements $ Cst True : Cst False : (Var <$> [0 .. n-1])

shrinkExp :: Exp -> [Exp]
shrinkExp (Atom atom) = Atom <$> shrink atom
shrinkExp (AND a b) = a : b : [AND a' b | a' <- shrink a] ++ [AND a b' | b' <- shrink b]
shrinkExp (XOR a b) = a : b : [XOR a' b | a' <- shrink a] ++ [XOR a b' | b' <- shrink b]
shrinkExp (OR a b) = a : b : [OR a' b | a' <- shrink a] ++ [OR a b' | b' <- shrink b]
shrinkExp (NEG a) = a : [NEG a'| a' <- shrink a]

shrinkAtom :: Atom -> [Atom]
shrinkAtom (Cst True) = []
shrinkAtom (Cst False) = []
shrinkAtom (Var n) = Var <$> shrink n