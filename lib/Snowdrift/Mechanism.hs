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
-- Module      : Snowdrift.Mechanism
-- Description : Mechanism for snowdrift
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 

module Snowdrift.Mechanism where

import Control.Monad.State.Lazy
import Data.Int (Int64)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S

-- |'Ident' is a unique numerical identification for an entry in a
-- database. 'Ident' is usually an 'Int', or some variety thereof.
-- 
-- This type exists to create an ostensible distinction between
-- integers-as-they-refer-to-some-other-thing and
-- integers-as-they-refer-to-identifiers.
type Ident = Int64

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
data Patron = Patron { patronFunds :: Int64
                     }
  deriving (Eq, Show)

-- |A 'Project' has some funds. There might be more attributes added later.
data Project = Project { projectFunds :: Int64
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


-- |The 'Pool' is the moral equivalent of a database, for the purposes of this
-- model.
data Pool = Pool { poolPatrons :: IdentMap Patron
                 , poolProjects :: IdentMap Project
                 , poolPledges :: Set Pledge
                 }
  deriving (Eq, Show)

-- |Possible errors when constructing pools
data PoolError = InvalidPledges PledgeError
  deriving (Eq, Show)

data PledgeError = NonexistentParties (Set Pledge)
  deriving (Eq, Show)

-- |"Smart constructor" for constructing pools. If 
mkPool :: IdentMap Patron
       -> IdentMap Project
       -> Set Pledge
       -> Either PoolError Pool
mkPool patrons projects pledges
  | not (S.null badPledges) = Left (InvalidPledges (NonexistentParties badPledges))
  | otherwise = Right (Pool patrons projects pledges)
  where
    potentialPool = Pool patrons projects pledges
    (goodPledges, badPledges) = verifyPledgePartiesExist potentialPool

-- |Looks through a 'Pool', and makes sure that the parties involved in each
-- 'Pledge' actually exist. It returns a pair of 'Set's. The first is the
-- good pledges, the second is the bad pledges.
verifyPledgePartiesExist :: Pool -> (Set Pledge, Set Pledge)
verifyPledgePartiesExist pl = snd (runState verification (mempty, mempty))
  where
    patronKeys = M.keys (poolPatrons pl)
    projectKeys = M.keys (poolProjects pl)
    verification = forM_ (poolPledges pl) $ \thisPledge@(Pledge ptr prj)-> do
      (goodPledges, badPledges) <- get
      if elem ptr patronKeys && elem prj projectKeys
        then put ( S.insert thisPledge goodPledges
                 , badPledges
                 )
        else put ( goodPledges
                 , S.insert thisPledge badPledges
                 )
        
        
-- |Given a 'Pool', see if the 'Ident's in a 'Pledge' refer to parties that
-- exist in the 'Pool'.
pledgePartiesExist :: Pool -> Pledge -> Bool
pledgePartiesExist (Pool patrons projects _) (Pledge patron project) =
  elem patron (M.keys patrons) && elem project (M.keys projects)
