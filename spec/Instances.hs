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

instance Arbitrary Pool where
  arbitrary = do
      patrons <- arbitrary
      projects <- arbitrary
      pledges <- createPledges patrons projects
      return (Pool patrons projects (mkValid pledges))
    where
      mkValid pledgeSet = Pledges pledgeSet mempty mempty

createPledges :: IdentMap Patron -> IdentMap Project -> Gen (Set Pledge)
createPledges patrons projects
  | M.null patrons || M.null projects = return mempty
  | otherwise = S.fromList <$> listOf (createPledge patrons projects)


-- |Input maps mustn't be empty
createPledge :: IdentMap Patron -> IdentMap Project -> Gen Pledge
createPledge patronsMap projectsMap = do
      -- select randomly from the patron ids
      patronId <- elements (M.keys patronsMap)
      -- select randomly from the project ids
      projectId <- elements (M.keys projectsMap)
      return (Pledge patronId projectId)
        

-- |A truly arbitrary pledge; the respective ids of the benefactor and the
-- beneficiary are generated at random, rather than picked from a list of
-- patrons/projects already in a pool.
genPledge :: Gen Pledge
genPledge = Pledge <$> arbitrary <*> arbitrary

-- |Given a Pool, generate a pledge that does not satisfy 'pledgePartiesExist'
badPledge :: Pool -> Gen Pledge
badPledge pool = suchThat genPledge (not . pledgePartiesExist pool)
