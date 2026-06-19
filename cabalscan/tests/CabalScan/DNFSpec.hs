{-# LANGUAGE OverloadedStrings #-}
module CabalScan.DNFSpec where

import Test.Hspec
import CabalScan.DNF
import qualified Data.Set as Set

spec_dnfSimplification :: Spec
spec_dnfSimplification =
  describe "DNF simplification" $ do
    it "should detect contradictions (x ∧ ¬x = False)" $ do
      let x = JustT (VarT ":x" "x")
          notX = NotT (VarT ":x" "x")
          contradiction = Disj $ Set.singleton $ Conj $ Set.fromList [x, notX]
      simplifyDNF contradiction `shouldBe` DnfFalse
    
    it "should apply identity law (True ∧ x = x)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfAnd DnfTrue x `shouldBe` x
    
    it "should apply identity law (x ∧ True = x)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfAnd x DnfTrue `shouldBe` x
    
    it "should apply annihilation law (False ∧ x = False)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfAnd DnfFalse x `shouldBe` DnfFalse
    
    it "should apply annihilation law (x ∧ False = False)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfAnd x DnfFalse `shouldBe` DnfFalse
    
    it "should apply OR identity law (False ∨ x = x)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfOr DnfFalse x `shouldBe` x
    
    it "should apply OR annihilation law (True ∨ x = True)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      dnfOr DnfTrue x `shouldBe` DnfTrue
    
    it "should apply absorption law ((a ∧ b) ∨ a = a)" $ do
      let a = JustT (VarT ":a" "a")
          b = JustT (VarT ":b" "b")
          aAndB = Conj $ Set.fromList [a, b]
          justA = Conj $ Set.singleton a
          dnfExpr = Disj $ Set.fromList [aAndB, justA]
      simplifyDNF dnfExpr `shouldBe` Disj (Set.singleton justA)
    
    it "should handle double negation (¬¬x = x)" $ do
      let x = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT ":x" "x")
      negateDNF (negateDNF x) `shouldBe` x
    
    it "should simplify tautology to True" $ do
      let tautology = Disj $ Set.singleton $ Conj Set.empty
      simplifyDNF tautology `shouldBe` DnfTrue
    
    it "should simplify empty disjunction to False" $ do
      let emptyDisj = Disj Set.empty
      simplifyDNF emptyDisj `shouldBe` DnfFalse
    
    it "should handle De Morgan's law: ¬(a ∧ b) = ¬a ∨ ¬b" $ do
      let a = JustT (VarT ":a" "a")
          b = JustT (VarT ":b" "b")
          aAndB = Disj $ Set.singleton $ Conj $ Set.fromList [a, b]
          notAOrNotB = Disj $ Set.fromList [
              Conj $ Set.singleton $ NotT (VarT ":a" "a"),
              Conj $ Set.singleton $ NotT (VarT ":b" "b")
            ]
      negateDNF aAndB `shouldBe` notAOrNotB

    it "should handle De Morgan's law: ¬(a ∨ b) = ¬a ∧ ¬b" $ do
      let a = JustT (VarT ":a" "a")
          b = JustT (VarT ":b" "b")
          aOrB = Disj $ Set.fromList [
              Conj $ Set.singleton a,
              Conj $ Set.singleton b
            ]
          notAAndNotB = Disj $ Set.singleton $ Conj $ Set.fromList [
              NotT (VarT ":a" "a"),
              NotT (VarT ":b" "b")
            ]
      negateDNF aOrB `shouldBe` notAAndNotB
