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
-- Module      : Snowdrift.Mech.Types
-- Description : The types for the Snowdrift mechanism
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains types and lenses for the mechanism.

module Snowdrift.Mech.Types where

import Snowdrift.Mech.Types.Funds
import Snowdrift.Mech.Types.Nat

import Control.Lens
import Data.Set (Set)
import Data.Vector (Vector)

-- ** Patrons and projects

-- |A 'Patron' has some funds, and a set of pledges.
data Patron = Patron { patronFunds :: Funds
                     , patronActivePledges :: Set Project
                     , patronInactivePledges :: Vector (Project, DisableReason)
                     }
  deriving (Show)


-- |A 'Project' has some funds, and a number of pledges.
data Project = Project { -- |We don't allow overdrawing
                         projectFunds :: Funds
                       , -- |Fewer than zero pledges doesn't make any sense. 
                         projectNumPledges :: Nat
                       }
  deriving (Show)

-- |The reason a pledge might have been disabled.
data DisableReason = NonexistentProject
                   | AccountZeroed
                   | UserRescinded
                   | PendingApproval
  deriving (Show)



-- ** Lenses

makeLensesFor
  [ ("patronFunds", "_patronFunds")
  , ("patronActivePledges", "_patronActivePledges")
  , ("patronInactivePledges", "_patronInactivePledges")
  ]
  ''Patron

makeLensesFor
  [ ("projectFunds", "_projectFunds")
  , ("projectNumPledges", "_projectNumPledges")
  ]
  ''Project

makeLenses ''DisableReason
