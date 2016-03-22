-- |Contains the definition of 'Project'
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
