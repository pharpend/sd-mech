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
-- Module      : PoolSpec
-- Description : Specifications for 'Pool's
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module PoolSpec where

import Instances
import Snowdrift.Mechanism

import Control.Monad (forM_)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  arbitraryCheck
  badPledgesCheck

-- |Checking arbitrarily-generated stuff is okay
arbitraryCheck :: Spec
arbitraryCheck =
    context "Arbitrary definitions don't produce illegal results" $
      context "Arbitrary pools" $ 
        arbitraryPoolPledgesCheck
  where
    arbitraryPoolPledgesCheck =
      context "Pledges in arbitrary pools shouldn't use IDs of nonexistent patrons or projects" $ do
        specify "All of the pledge patrons should exist" $ do
          property $ \pool -> do
            let patronIds = M.keys (poolPatrons pool)
                pledgePatrons = S.map pledgePatron (poolPledges pool)
            forM_ pledgePatrons $ \pledger ->
              shouldSatisfy pledger (`elem` patronIds)
        specify "All of the pledge projects should exist" $ do
          property $ \pool -> do
            let projectIds = M.keys (poolProjects pool)
                pledgeProjects = S.map pledgeProject (poolPledges pool)
            forM_ pledgeProjects $ \pledger ->
              shouldSatisfy pledger (`elem` projectIds)

        context "verifyPledgePartiesExist" $ do
          specify "The list of bad pledges should be empty" $
            property $ \pool -> do
              let (_, badPledges) = verifyPledgePartiesExist pool
              shouldSatisfy badPledges S.null
          specify "The list of good pledges should be the same length as the initial list" $
            property $ \pool -> do
              let (goodPledges, _) = verifyPledgePartiesExist pool
              shouldBe (S.size goodPledges)
                        (S.size (poolPledges pool))
        context "mkPool" $ do
          it "should return Right (whatever pool we generated)" $
            property $ \pool@(Pool patrons projects pledges) ->
              shouldBe (mkPool patrons projects pledges)
                       (Right pool)
        context "pledgePartiesExist" $ do
          it "should return true for each pledge in the pool" $
            property $ \pool ->
              forM_ (poolPledges pool) $ \pledge ->
                shouldSatisfy pledge (pledgePartiesExist pool)
            

-- |If we create some bad pledges, then the program should freak out
badPledgesCheck :: Spec
badPledgesCheck = context "Bad pledges" $ do
  context "Given an arbitrary pool" $ do
    context "Add a bunch of invalid pledges" $ do
      context "pledgePartiesExist" $ do
        it "should return false for each of our invalid pledges" $
          property $ \pool -> do
            badPledges <- S.fromList <$> generate (listOf (badPledge pool))
            forM_ badPledges $ \bp ->
              shouldSatisfy bp (not . pledgePartiesExist pool)
      context "mkPool (with a nonempty list of bad pledges)" $ do
        it "should return an error with a list of our invalid pledges" $
          property $ \pool@(Pool patrons projects pledges) -> do
            badPledges <- S.fromList <$> generate (listOf1 (badPledge pool))
            shouldBe (mkPool patrons projects (mappend pledges badPledges))
                     (Left (InvalidPledges (NonexistentParties badPledges)))
    
