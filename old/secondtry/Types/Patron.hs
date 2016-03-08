-- |This contains the definition of 'Patron'

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
