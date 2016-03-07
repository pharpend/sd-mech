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
-- Module      : Snowdrift.Mechanism.Pool
-- Description : Constructing and checking pools
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism.Pool where

import Snowdrift.Mechanism.Pledge
import Snowdrift.Mechanism.Types

import Data.Set (Set)
import qualified Data.Set as S

-- |"Smart constructor" for constructing pools.
-- 
-- Currently, it just suspends all pledges pending approval by the supreme
-- soviet.
mkPool :: IdentMap Patron
       -> IdentMap Project
       -> Set Pledge
       -> Pool
mkPool patrons projects pledges =
  Pool patrons
       projects
       (Pledges mempty
                (S.map PendingApproval pledges)
                mempty)
