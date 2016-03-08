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
import Snowdrift.Mechanism.Types.Lenses

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- |This is pretty complicated. Essentially, it takes a 'Pool', and makes sure
-- the structure of 'poolPledges' is correct. It returns the correct structure.
-- 
-- This is a rather inefficient operation.
checkPledges :: Pool -> Pledges
checkPledges pool =
                          
  where
    cleaveValidPledgesWithoutFunds pool pledges =
      snd (flip runState )
    -- takes 'nonDeleted', maps 'PendingApproval' onto it
    pledgesPendingApproval = S.map PendingApproval nonDeleted
    patronFunds pool pledge =
      case M.lookup (pledgePatron pledge) (poolPatrons pool) of
        Nothing -> error "Somehow, a patron both exists and does not. This means that everything in logic is legal! Yay! Actually, it's bad, so I'm going to go ahead and die."
        Just (Patron fs) -> fs
  
-- |Take all of the pledges, make sure they have funds. If they don't, remove it
-- from the set.
-- 
-- This throws an error if a pledge refers to a patron that doesn't exist.
cleavePoorPeople :: IdentMap Patron -> Set Pledge -> Set Pledge
cleavePoorPeople patrons pledges =
    for pledges $ \pledge -> 
      if patronFunds patrons pledge < S.size
  where for = flip fmap
        patronFunds' patrons pledge =
          patronFunds (lookupUnsafe patrons (pledgePatron pledge))
        lookupUnsafe k m =
          case M.lookup k m of
            Nothing -> error "Unsafe map lookup failed"
            Just x -> x

-- |All pledges that are *not* deleted or rescinded
nonDeleted :: Pledges -> Set Pledge
nonDeleted pledges' =
  pledgesValid pledges' `S.union`
    S.map unSuspension (pledgesSuspended pledges')

-- |Merge a 'Pledges' record into a 'Set' of 'Pledge's.
mergePledges :: Pledges -> Set Pledge
mergePledges (Pledges v s d) =
    S.unions [ v
             , S.map unDeletion d
             , S.map unSuspension s
             ]


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


-- -- * Lens-ish things

-- -- |Given a pool, produce a map from each patron's 'Ident' to Pledges with that
-- -- patron as benefactor.
-- -- 
-- -- Because it's actually much easier to do so, this goes ahead and effectively
-- -- runs 'fixPledges' on the pledges.
-- patronsToPledgesMap :: Pool -> Map PatronIdent Pledges
-- patronsToPledgesMap pool@(Pool patrons _ pledges) =
--     M.mapWithKey (\k _ -> mkPledges pool (pledgesFromPatron k))
--                  patrons
--   where
--     pledgesFromPatron patronId =
--       S.filter (\(Pledge patronId' _) -> patronId == patronId')
--                (mergePledges pledges)
    
-- -- |Given a pool, produce a map from each project's 'Ident' to Pledges with that
-- -- project as beneficiary.
-- projectsToPledgesMap :: Pool -> Map ProjectIdent Pledges
-- projectsToPledgesMap pool@(Pool _ projects pledges) =
--     M.mapWithKey (\k _ -> mkPledges pool (pledgesToProject k)) projects
--   where
--     pledgesToProject projectIdent =
--       S.filter (\(Pledge _ projectIdent') -> projectIdent == projectIdent')
--                (mergePledges pledges)
