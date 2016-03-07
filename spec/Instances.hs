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
-- Module      : Instances
-- Description : 'Arbitrary' instances
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains typeclass instance declarations for various types found
-- throughout the library. Primarily, these are instances of 'Arbitrary', from
-- "Test.QuickCheck".
-- 
-- NB: we don't create an 'Arbitrary' instance for 'Pledge's; these depend on
-- the id-numbers of the Patrons and Projects.

module Instances where

import Snowdrift.Mechanism

import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Test.QuickCheck

instance (Arbitrary x, Ord x) => Arbitrary (Set x) where
  arbitrary = fmap S.fromList arbitrary         

instance Arbitrary x => Arbitrary (IdentMap x) where
  arbitrary = M.fromList <$> arbitrary         

instance Arbitrary Patron where
  arbitrary = fmap Patron arbitrary

instance Arbitrary Project where
  arbitrary = fmap Project arbitrary

-- |Create both valid and invalid pledges
createSomePledges :: IdentMap Patron -> IdentMap Project -> Gen (Set Pledge)
createSomePledges patrons projects = do
  validPledgesSet <- createValidPledges patrons projects
  invalidPatronPledgesList <- listOf $ invalidPatronPledge patrons
  invalidProjectPledgesList <- listOf $ invalidProjectPledge projects
  return (S.unions [ validPledgesSet
                   , S.fromList invalidProjectPledgesList
                   , S.fromList invalidPatronPledgesList
                   ])

-- |Creates only valid pledges
createValidPledges :: IdentMap Patron -> IdentMap Project -> Gen (Set Pledge)
createValidPledges patrons projects
  | M.null patrons || M.null projects = return mempty
  | otherwise = S.fromList <$> listOf (createValidPledge patrons projects)


-- |Input maps mustn't be empty
-- 
-- This creates a valid pledge, given a set of patrons and projects
createValidPledge :: IdentMap Patron -> IdentMap Project -> Gen Pledge
createValidPledge patronsMap projectsMap = do
      -- select randomly from the patron ids
      patronId <- elements (M.keys patronsMap)
      -- select randomly from the project ids
      projectId <- elements (M.keys projectsMap)
      return (Pledge patronId projectId)
        

-- |A truly arbitrary pledge; the respective ids of the benefactor and the
-- beneficiary are generated at random, rather than picked from a list of
-- patrons/projects already in a pool.
createPledge :: Gen Pledge
createPledge = Pledge <$> arbitrary <*> arbitrary

-- |Given a bunch of patrons, generate a pledge from a nonexistent patron
invalidPatronPledge :: IdentMap Patron -> Gen Pledge
invalidPatronPledge map' =
  createPledge `suchThat`
    \(Pledge patron _) ->
      not $ patron `elem` M.keys map'

-- |Given a bunch of projects, generate a pledge from a nonexistent project
invalidProjectPledge :: IdentMap Project -> Gen Pledge
invalidProjectPledge map' = do
  createPledge `suchThat`
    \(Pledge _ project) ->
      not $ project `elem` M.keys map'
