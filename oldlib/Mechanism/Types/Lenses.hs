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
-- Module      : Snowdrift.Mechanism.Types.Lenses
-- Description : Lenses for the various mechanism types
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism.Types.Lenses where

import Snowdrift.Mechanism.Types

import Control.Lens
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as S

-- |Little wrapper to represent the size of a set as an 'Int64'
sizeOfSet :: Set x -> Int64
sizeOfSet = fromIntegral . S.size

-- ** 'Pool's
makeLensesFor
  [ ("poolPatrons", "_poolPatrons")
  , ("poolProjects", "_poolProjects")
  , ("poolPledges", "_poolPledges")
  ]
  ''Pool

-- ** 'Patron's
makeLensesFor
  [ ("patronFunds", "_patronFunds")
  ]
  ''Patron

-- ** 'Project's
makeLensesFor
  [ ("projectFunds", "_projectFunds")
  ]
  ''Project

-- ** 'Pledges'
makeLensesFor
    [ ("pledgesValid", "_pledgesValid")
    , ("pledgesSuspended", "_pledgesSuspended")
    , ("pledgesDeleted", "_pledgesDeleted")
    ]
    ''Pledges

-- -- |Sort of like fmap, but for something that isn't technically a functor
-- mutatePledges :: (Pledge -> Pledge) -> Pledges -> Pledges
-- mutatePledges f (Pledges v s d) =
--   Pledges (fmap f v)
--           (fmap (PledgeSuspension . f . unSuspension) s)
--           (fmap (PledgeDeletion . f . unDeletion) d)

-- ** 'Pledge's
makeLensesFor
  [ ("pledgePatron", "_pledgePatron")
  , ("pledgeProject", "_pledgeProject") 
  ]
  ''Pledge
