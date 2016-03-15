-- |This has to go in its own module, because of silly GHC stage restrictions.
module SdMech.Types.PledgeStatus where

import Control.Lens.TH
import Database.Persist.TH

data PledgeStatus = StActive
                  | StPatronSuspended
                  | StImpoverishedPatron
    deriving (Eq, Read, Show)

derivePersistField "PledgeStatus"
makePrisms ''PledgeStatus
