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
-- Module      : Snowdrift.Mechanism.Pledge
-- Description : Functions for checking attributes of pledges
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism.Pledge.Checks where

import Snowdrift.Mechanism.Types

import qualified Data.Map as M

-- |Make sure both parties exist in a pledge. Equivalent to and-ing
-- 'pledgePatronExists' and 'pledgeProjectExists'.
pledgePartiesExist :: Pool -> Pledge -> Bool
pledgePartiesExist pool pledge =
  pledgePatronExists pool pledge && pledgeProjectExists pool pledge

-- |Check to see if the Pledge's patron is in the pool
pledgePatronExists :: Pool -> Pledge -> Bool
pledgePatronExists (Pool patrons _ _) (Pledge patron _) =
  elem patron (M.keys patrons)

-- |Check to see if the Pledge's project is in the pool
pledgeProjectExists :: Pool -> Pledge -> Bool
pledgeProjectExists (Pool _ projects _) (Pledge _ project) =
  elem project (M.keys projects)
