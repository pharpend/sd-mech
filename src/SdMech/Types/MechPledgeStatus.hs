-- |This has to go in its own module, because of silly GHC stage restrictions.
module SdMech.Types.MechPledgeStatus where

import Control.Lens.TH
import Database.Persist.TH

data MechPledgeStatus = StActive
                      | StPatronSuspended
                      | StImpoverishedPatron
    deriving (Eq, Read, Show)

derivePersistField "MechPledgeStatus"
makePrisms ''MechPledgeStatus
