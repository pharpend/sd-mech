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
-- Module      : NatSpec
-- Description : Tests for the 'Nat' type
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module NatSpec where

import Instances ()
import Snowdrift.Mech

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
                property $ \n ->
                    (n <+> zero) `shouldBe` (n :: Nat)
            specify "Symmetry: forall n m, m + n = n + m" $ 
                property $ \(n, m) ->
                    shouldBe (n <+> m) (m <+> n :: Nat)
            specify "Associativity: forall a b c, (a + b) + c = a + (b + c)" $ 
                property $ \(a, b, c) ->
                shouldBe ((a <+> b) <+> c) (a <+> (b <+> c) :: Nat)
        context "Subtraction" $ do
            specify "Identity: forall n, n - 0 = n" $ 
                property $ \n ->
                    (n <-> zero) `shouldBe` n
            specify "Antiassociativity: forall a b c, (a - b) - c = a - (b + c)" $ 
                property $ \(a, b, c) ->
                shouldBe ((a <-> b) <-> c) (a <-> (b <+> c))
