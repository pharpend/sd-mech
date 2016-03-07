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

import Snowdrift.Mechanism.Types

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- |Given a pool, produce a map from each patron's 'Ident' to Pledges with that
-- patron as benefactor.
-- 
-- Because it's actually much easier to do so, this goes ahead and effectively
-- runs 'fixPledges' on the pledges.
patronsPledges :: Pool -> Map PatronIdent Pledges
patronsPledges pool@(Pool patrons _ pledges) =
    M.mapWithKey (\k _ -> mkPledges pool (pledgesFromPatron k)) patrons
  where
    pledgesFromPatron patronId =
      S.filter (\(Pledge patronId' _) -> patronId == patronId')
               (mergePledges pledges)
    

-- |Given a pool, produce a map from each project's 'Ident' to Pledges with that
-- project as beneficiary.
projectsPledges :: Pool -> Map ProjectIdent Pledges
projectsPledges pool@(Pool _ projects pledges) =
    M.mapWithKey (\k _ -> mkPledges pool (pledgesToProject k)) projects
  where
    pledgesToProject projectIdent =
      S.filter (\(Pledge _ projectIdent') -> projectIdent == projectIdent')
               (mergePledges pledges)


-- |Given a 'Pool', and the 'Pledges', "fix" them, so to speak. For instance, if
-- a patron is listed as having insufficient funds, but is discovered to have
-- sufficient funds, this will fix any errors like that.
fixPledges :: Pool -> Pledges -> Pledges
fixPledges pool pledges = mkPledges pool (mergePledges pledges)

-- |Given a 'Pool', and a 'Set' of 'Pledge's, split the set into 'Pledges',
-- meaning split them up into valid pledges, suspended pledges, and pledges to
-- delete.
mkPledges :: Pool -> Set Pledge -> Pledges
mkPledges = undefined

-- |Left inverse of 'mkPledges'. This doesn't require the pool for verification,
-- so it's left out.
mergePledges :: Pledges -> Set Pledge
mergePledges (Pledges v s d) = S.unions [ v
                                        , S.map unDeletion d
                                        , S.map unSuspension s
                                        ]

unDeletion :: PledgeDeletion -> Pledge  
unDeletion =
  \case
    NonexistentPatron p -> p
    NonexistentProject p -> p
    PatronRescinded p -> p

unSuspension :: PledgeSuspension -> Pledge  
unSuspension =
  \case
    InsufficientFunds p -> p
    PatronSuspended p -> p

-- |Given a 'Pool', see if the 'Ident's in a 'Pledge' refer to parties that
-- exist in the 'Pool'.
pledgePartiesExist :: Pool -> Pledge -> Bool
pledgePartiesExist pool pledge = 
  and [ pledgePatronExists pool pledge
      , pledgeProjectExists pool pledge
      ]

-- |Check to see if the Pledge's patron is in the pool
pledgePatronExists :: Pool -> Pledge -> Bool
pledgePatronExists (Pool patrons _ _) (Pledge patron _) =
  elem patron (M.keys patrons)

-- |Check to see if the Pledge's project is in the pool
pledgeProjectExists :: Pool -> Pledge -> Bool
pledgeProjectExists (Pool _ projects _) (Pledge _ project) =
  elem project (M.keys projects)
