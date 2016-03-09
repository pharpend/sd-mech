-- |Tests for the 'Funds' type
module FundsSpec where

import Instances ()
import SdMech

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  context "Lemmas about natural numbers" $ do
    context "'Depositing' (i.e. Addition)" $ do
      specify "Identity: forall n, n + 0 = n" $ 
        property $ \n ->
          (n <+> zero) `shouldBe` (n :: Funds)
      specify "Symmetry: forall n m, m + n = n + m" $ 
        property $ \(n, m) ->
          (n <+> m) `shouldBe` (m <+> n :: Funds)
      specify "Associativity: forall a b c, (a + b) + c = a + (b + c)" $ 
          property $ \(a, b, c) ->
            ((a <+> b) <+> c) `shouldBe` (a <+> (b <+> c) :: Funds)
    context "'Withdrawing' (i.e. subtraction); bw = balance after withdrawal" $ do
      let bw n m = balanceAfter (withdraw n m)
      specify "Identity: forall n, bw n zero = n" $ 
        property $ \n ->
          bw n zero `shouldBe` n
      specify "Antiassociativity: forall a b c, (bw (bw a b) c) = (bw a (b + c))" $ 
        property $ \(a, b, c) ->
          (bw (bw a b) c) `shouldBe `(bw a (b <+> c))
