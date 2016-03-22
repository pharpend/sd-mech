-- |Tests for the 'Nat' type
module NatSpec where

import Instances ()
import SdMech

import Prelude hiding (subtract, succ, pred)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  context "Lemmas about natural numbers" $ do
    specify "pred . succ = id" $ 
      property $ \n -> pred (succ n) `shouldBe` n
    context "Addition" $ do
      specify "Identity: forall n, n + 0 = n" $ 
        property $ \n -> (n <+> zero) `shouldBe` (n :: Nat)
      specify "Symmetry: forall n m, m + n = n + m" $ 
        property $ \(n, m) -> shouldBe (n <+> m) (m <+> n :: Nat)
      specify "Associativity: forall a b c, (a + b) + c = a + (b + c)" $ 
        property $ \(a, b, c) ->
            shouldBe ((a <+> b) <+> c) (a <+> (b <+> c) :: Nat)
    context "Subtraction" $ do
      specify "Identity: forall n, n - 0 = n" $ 
        property $ \n -> (n <-> zero) `shouldBe` n
      specify "Antiassociativity: forall a b c, (a - b) - c = a - (b + c)" $ 
        property $ \(a, b, c) ->
          shouldBe ((a <-> b) <-> c) (a <-> (b <+> c))
