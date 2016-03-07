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

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.QuickCheck

instance Arbitrary x => Arbitrary (Vector x) where
  arbitrary = fmap V.fromList arbitrary         

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
      return (Pool patrons projects pledges)

createPledges :: IdentMap Patron -> IdentMap Project -> Gen (Vector Pledge)
createPledges patrons projects
  | M.null patrons || M.null projects = return mempty
  | otherwise = V.fromList <$> listOf (createPledge patrons projects)


-- |Input maps mustn't be empty
createPledge :: IdentMap Patron -> IdentMap Project -> Gen Pledge
createPledge patronsMap projectsMap = do
      -- select randomly from the patron ids
      patronId <- elements (M.keys patronsMap)
      -- select randomly from the project ids
      projectId <- elements (M.keys projectsMap)
      return (Pledge patronId projectId)
        
