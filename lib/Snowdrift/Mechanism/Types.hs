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
-- Module      : Snowdrift.Mechanism.Types
-- Description : The types for the Snowdrift mechanism
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism.Types where

import Control.Monad.State
import Data.Int (Int64)
import Data.Ord (comparing)
import Data.Map.Lazy (Map)
import Data.Set (Set)

-- |The 'Pool' is the moral equivalent of a database, for the purposes of this
-- model.
data Pool = Pool { poolPatrons :: IdentMap Patron
                 , poolProjects :: IdentMap Project
                 , poolPledges :: Pledges
                 }
  deriving (Eq, Show)

-- |'Ident' is a unique numerical identification for an entry in a
-- database. 'Ident' is usually an 'Int', or some variety thereof.
-- 
-- This type exists to create an ostensible distinction between
-- integers-as-they-refer-to-some-other-thing and
-- integers-as-they-refer-to-identifiers.
type Ident = Int64
type PatronIdent = Int64
type ProjectIdent = Int64
-- |Funds are represented as a 64-bit integer for performance reasons
type Funds = Int64

-- |A 'Map' from 'Ident's to whatever
type IdentMap = Map Ident

-- |A 'Patron' has some funds. There might be more attributes added later.
-- 
-- Note that the funds are represented as an 'Int64'. This is something
-- Snowdrift-specific-ish. Essentially, to avoid floating-point arithmetic,
-- funds are represented as a number of 'mills'. A 'mill' is some fraction of a
-- cent. The point is this avoids nonsense like this
-- 
-- >>> 0.1 + 0.2
-- 0.30000000000000004
-- >>>
-- 
-- (Seriously, try it in GHCi).
data Patron = Patron { patronFunds :: Funds
                     }
  deriving (Eq, Show)

-- |A 'Project' has some funds. There might be more attributes added later.
data Project = Project { projectFunds :: Funds
                       }
  deriving (Eq, Show)


-- |The set of pledges for a 'Pool'. These ought to be constructed such that
-- their union is null.
data Pledges = Pledges { pledgesValid :: Set Pledge
                       , pledgesSuspended :: Set PledgeSuspension
                       , pledgesDeleted :: Set PledgeDeletion
                       }
  deriving (Eq, Show)

-- |A pledge is from a patron to a project. The amount is calculated based on
-- how many other patrons also pledged
data Pledge = Pledge { pledgePatron :: Ident
                     , pledgeProject :: Ident
                     }
  deriving (Eq, Show)

instance Ord Pledge where
  compare (Pledge a1 a2) (Pledge b1 b2) = compare (a1, a2) (b1, b2)


-- |Reasons to delete a pledge
data PledgeDeletion = NonexistentPatron Pledge
                    | NonexistentProject Pledge
  deriving (Eq, Show)

-- |Get the pledge that was deleted for whatever reason
unDeletion :: PledgeDeletion -> Pledge  
unDeletion = \case
  NonexistentPatron p -> p
  NonexistentProject p -> p

instance Ord PledgeDeletion where
  compare = comparing unDeletion

-- |Reasons to suspend a pledge
data PledgeSuspension = InsufficientFunds Pledge
                      | PendingApproval Pledge
  deriving (Eq, Show)

-- |Get the underlying pledge that was suspended for whatever reason.
unSuspension :: PledgeSuspension -> Pledge
unSuspension = \case
  InsufficientFunds p -> p
  PendingApproval p -> p

instance Ord PledgeSuspension where
  compare = comparing unSuspension

-- |Monadic representations of pools. Not in use yet, but perhaps at some point
-- in the future
type PoolT m x = StateT Pool m x
type PoolM x = State Pool x
