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
-- Module      : Snowdrift.Mech.Types.Project
-- Description : The 'Project' type
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mech.Types.Project where

import Snowdrift.Mech.Types.Funds
import Snowdrift.Mech.Types.Nat

import Control.Lens
import Data.Aeson
import Data.Ord (comparing)
import Data.Text (Text)
import Database.Persist.TH
import GHC.Generics

-- |A 'Project' has some funds, and a number of pledges.
data Project = Project { projectHandle :: Text
                       , -- |We don't allow overdrawing
                         projectFunds :: Funds
                       , -- |Fewer than zero pledges doesn't make any sense. 
                         projectNumPledges :: Nat
                       }
  deriving (Show, Generic)

instance Eq Project where
  (==) p1 p2 = projectHandle p1 == projectHandle p2

instance Ord Project where
  compare = comparing projectHandle

-- ** Lens
makeLensesWith camelCaseFields ''Project

-- Aeson
instance ToJSON Project where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Project

-- Persistent
derivePersistFieldJSON "Project"
