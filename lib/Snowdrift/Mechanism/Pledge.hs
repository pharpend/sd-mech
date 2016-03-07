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
-- Description : Constructing and checking pledges
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism.Pledge where

import Snowdrift.Mechanism.Pledge.Checks
import Snowdrift.Mechanism.Pledge.Lenses
import Snowdrift.Mechanism.Types

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- |Given a 'Pool', and a 'Set' of 'Pledge's, split the set into 'Pledges',
-- meaning split them up into valid pledges, suspended pledges, and pledges to
-- delete.
mkPledges :: Pool -> Set Pledge -> Pledges
mkPledges pool pledgeSet =
  undefined

-- |Left inverse of 'mkPledges'. This doesn't require the pool for verification,
-- so it's left out.
mergePledges :: Pledges -> Set Pledge
mergePledges (Pledges v s d) =
    S.unions [ v
             , S.map unDeletion d
             , S.map unSuspension s
             ]
  where
    unSuspension = \case
      InsufficientFunds p -> p
    unDeletion = \case
      NonexistentPatron p -> p
      NonexistentProject p -> p

-- |Given a pool, produce a map from each patron's 'Ident' to Pledges with that
-- patron as benefactor.
-- 
-- Because it's actually much easier to do so, this goes ahead and effectively
-- runs 'fixPledges' on the pledges.
patronsToPledgesMap :: Pool -> Map PatronIdent Pledges
patronsToPledgesMap pool@(Pool patrons _ pledges) =
    M.mapWithKey (\k _ -> mkPledges pool (pledgesFromPatron k))
                 patrons
  where
    pledgesFromPatron patronId =
      S.filter (\(Pledge patronId' _) -> patronId == patronId')
               (mergePledges pledges)
    
-- |Given a pool, produce a map from each project's 'Ident' to Pledges with that
-- project as beneficiary.
projectsToPledgesMap :: Pool -> Map ProjectIdent Pledges
projectsToPledgesMap pool@(Pool _ projects pledges) =
    M.mapWithKey (\k _ -> mkPledges pool (pledgesToProject k)) projects
  where
    pledgesToProject projectIdent =
      S.filter (\(Pledge _ projectIdent') -> projectIdent == projectIdent')
               (mergePledges pledges)


-- |Get the pledge patron's id, then look him up
getPledgePatron :: Pool -> Pledge -> Maybe Patron
getPledgePatron pool pledge =
  M.lookup (pledgePatron pledge) (poolPatrons pool)

-- |Get the pledge benefactor's id, then look them up
getPledgeProject :: Pool -> Pledge -> Maybe Project
getPledgeProject pool pledge =
  M.lookup (pledgeProject pledge) (poolProjects pool)
