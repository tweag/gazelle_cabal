{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Disjunctive Normal Form (DNF) representation and simplification
module CabalScan.DNF
  ( -- * Types
    DNF(..), Conj(..), Term(..), VarT(..)
    -- * Patterns
  , pattern DnfTrue, pattern DnfFalse
  , pattern SinglePosVar, pattern SingleNegVar, pattern SinglePosVarTerm
    -- * Operations
  , simplifyDNF, dnfAnd, dnfOr, negateDNF
    -- * Helpers
  , termVar, termShortName, isNegTerm, simplifyConj
  ) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- from https://stackoverflow.com/a/73921595
newtype DNF  = Disj (Set Conj) deriving (Show, Eq, Ord)  -- DNF is a disjunction
newtype Conj = Conj (Set Term) deriving (Show, Eq, Ord)  -- of conjunctions
data    Term = JustT VarT
             | NotT VarT   deriving (Show, Eq, Ord)      -- of possibly negated
data VarT = VarT                                         -- variables
  { varLabel :: String  -- Full Bazel label (e.g., "@platforms//os:linux")
  , varShortName :: String  -- Short name for display (e.g., "linux", "network-support")
  } deriving (Show, Eq, Ord)

-- Pattern synonyms for easier pattern matching

-- Contradiction (empty disjunction)
pattern DnfFalse :: DNF
pattern DnfFalse <- Disj (null -> True) where
  DnfFalse = Disj Set.empty

-- Tautology (empty conjunction)
pattern DnfTrue :: DNF
pattern DnfTrue <- Disj (Set.toList -> [Conj (null -> True)]) where
  DnfTrue = Disj $ Set.singleton (Conj Set.empty)

pattern SinglePosVar :: String -> Conj
pattern SinglePosVar v <- Conj (Set.toList -> [JustT (VarT v _)])

pattern SingleNegVar :: String -> String -> Term
pattern SingleNegVar lbl short = NotT (VarT lbl short)

pattern SinglePosVarTerm :: String -> String -> Term
pattern SinglePosVarTerm lbl short = JustT (VarT lbl short)

-- Helper to extract variable label from a term
termVar :: Term -> String
termVar (JustT (VarT lbl _)) = lbl
termVar (NotT (VarT lbl _)) = lbl

-- Helper to extract variable short name from a term
termShortName :: Term -> String
termShortName (JustT (VarT _ short)) = short
termShortName (NotT (VarT _ short)) = short

isNegTerm :: Term -> Bool
isNegTerm (NotT _) = True
isNegTerm _ = False

-- Simplify a DNF expression by applying logical laws
simplifyDNF :: DNF -> DNF
simplifyDNF (Disj conjs)
  | Set.null validConjs = DnfFalse  -- Empty disjunction is False
  | any isEmptyConj validConjs = DnfTrue  -- Law of excluded middle: x ∨ ¬x = True
  | otherwise = Disj $ absorb validConjs
  where
    -- Filter out contradictions (x ∧ ¬x = False)
    validConjs = Set.fromList $ catMaybes $ map simplifyConj $ Set.toList conjs

    isEmptyConj (Conj terms) = Set.null terms

    -- Absorption: remove conjunctions that are subsumed by others
    -- If conjunction A ⊆ B, then A ∨ B = A
    absorb cs = Set.filter isMinimal cs
      where
        isMinimal conj@(Conj terms) =
          not $ any (\(Conj otherTerms) ->
            conj /= Conj otherTerms && otherTerms `Set.isProperSubsetOf` terms) cs

-- Simplify a conjunction by removing contradictions
simplifyConj :: Conj -> Maybe Conj
simplifyConj (Conj terms)
  | any hasComplement terms = Nothing  -- Contradiction: x ∧ ¬x = False
  | otherwise = Just (Conj terms)
  where
    hasComplement t = case t of
      JustT v -> NotT v `Set.member` terms
      NotT v -> JustT v `Set.member` terms

-- AND operation on DNF expressions
dnfAnd :: DNF -> DNF -> DNF
dnfAnd DnfFalse _ = DnfFalse  -- False ∧ x = False
dnfAnd _ DnfFalse = DnfFalse  -- x ∧ False = False
dnfAnd DnfTrue x = x          -- True ∧ x = x
dnfAnd x DnfTrue = x          -- x ∧ True = x
dnfAnd (Disj conjs1) (Disj conjs2) = simplifyDNF $ Disj $ Set.fromList $
  mapMaybe combineConj $ Set.toList $ Set.cartesianProduct conjs1 conjs2
  where
    combineConj (Conj ts1, Conj ts2) =
      let combined = Conj (Set.union ts1 ts2)
      in simplifyConj combined

-- OR operation on DNF expressions
dnfOr :: DNF -> DNF -> DNF
dnfOr DnfFalse x = x          -- False ∨ x = x
dnfOr x DnfFalse = x          -- x ∨ False = x
dnfOr DnfTrue _ = DnfTrue     -- True ∨ x = True
dnfOr _ DnfTrue = DnfTrue     -- x ∨ True = True
dnfOr (Disj d1) (Disj d2) = simplifyDNF $ Disj (d1 `Set.union` d2)

-- Negation of DNF expressions (applies De Morgan's laws)
-- ¬(a ∨ b) = ¬a ∧ ¬b
-- ¬(a ∧ b) = ¬a ∨ ¬b
negateDNF :: DNF -> DNF
negateDNF DnfTrue = DnfFalse   -- ¬True = False
negateDNF DnfFalse = DnfTrue   -- ¬False = True
negateDNF (Disj cs) =
  -- Apply De Morgan: ¬(C1 ∨ C2 ∨ ...) = ¬C1 ∧ ¬C2 ∧ ...
  -- Each ¬Ci is a disjunction (since Ci is a conjunction)
  Set.foldl dnfAnd DnfTrue $ Set.map negateConj cs
  where
    -- Negate a conjunction: ¬(a ∧ b) = ¬a ∨ ¬b
    negateConj (Conj ts) = Disj $ Set.map (\t -> Conj $ Set.singleton $ negateTerm t) ts
    negateTerm = \case
      JustT v -> NotT v
      NotT v -> JustT v
