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
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
    arbitraryCheck

-- |Checking arbitrarily-generated stuff is okay
arbitraryCheck :: Spec
arbitraryCheck = context "Arbitrary generation" $ do
    context "Generation of 'valid' pledges" $  do
        context "createValidPledges" $ do
            it "should generate pledges that refer to existent patrons" $
                property $ \(patrons, projects) -> do
                    pledges <- generate $ createValidPledges patrons projects
                    forM_ pledges $ \pledge ->
                        pledge `shouldSatisfy`
                            \(Pledge ptrid _) ->
                                ptrid `elem` M.keys patrons
            it "should generate pledges that refer to existent projects" $
                property $ \(patrons, projects) -> do
                    pledges <- generate $ createValidPledges patrons projects
                    forM_ pledges $ \pledge ->
                        pledge `shouldSatisfy`
                            \(Pledge _ prjid) ->
                                prjid `elem` M.keys projects

    context "Generation of invalid pledges" $
      specify "there should be tests here" $
        pendingWith "Laziness"

    context "Pledges in arbitrary pools" $ do
        context "pledgesValid (poolPledges pool)" $ do
            context "Patrons" $ do
                specify "they should all exist" $
                    pendingWith "Arbitrary instance for Pool"
                specify "they should all have sufficient funds" $
                    pendingWith "Pool construction & verification in library"
            specify "All of the pledge projects should exist" $
                pendingWith "Arbitrary instance for Pool"

        context "mkPool" $ do
            it "should create the structure of the Pledges record" $
                pendingWith "Nontrivial implementation"

        context "fixPledges" $ do
            it "should correct the structure of the Pledges record" $
                pendingWith "Nontrivial implementation"

        context "mergePledges" $ do
            it "should take every pledge in every orifice, flatten it out into one set" $
                pendingWith "Laziness"
