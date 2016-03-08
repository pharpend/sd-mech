-- Snowdrift.coop - cooperative funding platform
-- Copyright (c) 2012-2016, Snowdrift.coop
-- 
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : FundsSpec
-- Description : Tests for the 'Funds' type
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module FundsSpec where

import Instances ()
import Snowdrift.Mech

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
