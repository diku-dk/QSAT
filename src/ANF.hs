module ANF where

import AST ( Exp(..), Atom(..) )
import Data.List (intercalate, union, nub)
import Data.Maybe (mapMaybe)
import Gates
import Test.QuickCheck
import Safe.Foldable

--- Type Definitions ---

newtype PolySystem = PS [ANF]
  deriving Show
type ANF = [ANFterm]
type ANFterm = [Atom]

instance Arbitrary PolySystem where
  arbitrary = PS <$> (listOf1 . listOf1 . listOf1) (sized genVar)
  -- Using ListOf1 for now and not using constants (genVar) as I don't want to deal with the strange edge cases

genVar :: Int -> Gen Atom
genVar n = elements $ Var <$> [0 .. n]

--- conversion Exp -> ANF ---

elimOrNeg :: Exp -> Exp
elimOrNeg (Atom (Cst b)) = Atom $ Cst b
elimOrNeg (Atom (Var i)) = Atom $ Var i
elimOrNeg (NEG e) = XOR (elimOrNeg e) $ Atom $ Cst True
elimOrNeg (XOR e1 e2) = XOR (elimOrNeg e1) (elimOrNeg e2)
elimOrNeg (AND e1 e2) = AND (elimOrNeg e1) (elimOrNeg e2)
elimOrNeg (OR e1 e2) =
  let e1' = elimOrNeg e1
      e2' = elimOrNeg e2
   in XOR (XOR e1' e2') (AND e1' e2') -- equivalent to elimORwXOR

-- distribute AND across XOR.
distributeAnd :: Exp -> Exp
distributeAnd e =
  case e of
    AND e1 e2 -> distOne (distributeAnd e1) (distributeAnd e2)
    XOR e1 e2 -> XOR (distributeAnd e1) (distributeAnd e2)
    _ -> e
  where
    distOne e1 e2 =
      case (e1,e2) of
        (XOR a b, c) -> XOR (distOne a c) (distOne b c)
        (a, XOR b c) -> XOR (distOne a b) (distOne a c)
        _ -> AND e1 e2

makeAnf :: Exp -> Maybe ANF
makeAnf (XOR a b) = (++) <$> makeAnf a <*> makeAnf b
makeAnf e@(AND _ _) = (:[]) <$> makeAnfTerm e
makeAnf e@(Atom _) = (:[]) <$> makeAnfTerm e
makeAnf _ = Nothing

makeAnfTerm :: Exp -> Maybe ANFterm
makeAnfTerm (AND a b) = union <$> makeAnfTerm a <*> makeAnfTerm b
makeAnfTerm (Atom atom) = Just [atom]
makeAnfTerm _ = Nothing

simplifyAnfTerm :: ANFterm -> Maybe ANFterm
simplifyAnfTerm term =
  if Cst False `elem` term
    then Nothing
    else Just $ filter (/= Cst True) $ nub term

simplifyAnf :: ANF -> Maybe ANF -- TODO: also make so that "a xor a = Nothing"
simplifyAnf anf = 
  let anf' = mapMaybe simplifyAnfTerm anf
  in if null anf' then Nothing else Just anf'

simplifyPs :: PolySystem -> Maybe PolySystem
simplifyPs (PS anfs) = PS <$> mapM simplifyAnf anfs

exp2anf :: Exp -> Maybe ANF
exp2anf e = makeAnf (distributeAnd $ elimOrNeg e)

--- Conversion ANF -> oracle ---

extractVar :: Atom -> Maybe Int
extractVar (Var i) = Just i
extractVar _ = Nothing

maxVar :: PolySystem -> QubitPos
maxVar (PS anfs) = maximumDef (-1) $ (concatMap . concatMap . mapMaybe) extractVar anfs

ps2width :: PolySystem -> CircuitDescriptor
ps2width ps@(PS anfs) = (maxVar ps + 1, length anfs)

anf2oracle :: QubitPos -> ANF -> Program
anf2oracle ancilla = map anfTerm2oracle
  where
    anfTerm2oracle :: ANFterm -> Gate
    anfTerm2oracle [] = Only ancilla X
    anfTerm2oracle l = MCX (mapMaybe extractVar l) ancilla

ps2oracle :: PolySystem -> Program
ps2oracle (PS anfs) =
  let
    ancillas = take (length anfs) [maxVar (PS anfs) + 1 ..]
    halfprogram = concat $ zipWith anf2oracle ancillas anfs
  in
    halfprogram ++ [MCZ ancillas] ++ reverse halfprogram

--- pretty printers ---

ppANF :: ANF -> String
ppANF anf = intercalate " +" (ppANFterm <$> anf)

ppANFterm :: ANFterm -> String
ppANFterm [] = "1"
ppANFterm l = concatMap ((' ':) . show) l
