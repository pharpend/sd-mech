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
-- Module      : Snowdrift.Mech.Types.Patron
-- Description : The types for the Snowdrift mechanism
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : GHC

module Snowdrift.Mech.Types.Patron where

import Snowdrift.Mech.Types.Funds
import Snowdrift.Mech.Types.Project

import Control.Lens
import Data.Aeson
import Data.Set (Set)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist.TH
import GHC.Generics

-- ** Patrons and projects

-- |A 'Patron' has some funds, and a set of pledges.
data Patron = Patron { patronHandle :: Text
                     , patronFunds :: Funds
                     , patronActivePledges :: Set Project
                     , patronInactivePledges :: Vector (Project, DisableReason)
                     }
  deriving (Show, Generic)

instance Eq Patron where
  (==) p1 p2 = patronHandle p1 == patronHandle p2

instance Ord Patron where
  compare = comparing patronHandle

-- |The reason a pledge might have been disabled.
data DisableReason = NonexistentProject
                   | AccountZeroed
                   | UserRescinded
                   | PendingApproval
  deriving (Eq, Show, Read, Generic)

-- ** Lens
makeLensesWith camelCaseFields ''Patron
makePrisms ''DisableReason

-- Aeson
instance ToJSON Patron where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Patron

instance ToJSON DisableReason where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON DisableReason

-- Persistent
derivePersistFieldJSON "Patron"
derivePersistFieldJSON "DisableReason"
