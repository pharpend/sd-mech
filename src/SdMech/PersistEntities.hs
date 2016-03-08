-- |Types that are built to be stored in a database.
module SdMech.PersistEntities where

import SdMech.Funds
import SdMech.Util

share [mkPersist sqlSettings, mkMigrate "migrateMech"]
      [persistLowerCase|
      MPatron
          funds Funds
          pledges MPledgeId Set
          externalKey Int
          ExternalPatron externalKey
      MProject
          funds Funds
          pledges MPledgeId Set
          externalKey Int
          ExternalProject externalKey
      MPledge
          patron MPatronId
          project MProjectId
          UniqueMPledge patron project
      |]

makeLensesWith camelCaseFields ''MPatron
makeLensesWith camelCaseFields ''MProject
makeLensesWith camelCaseFields ''MPledge
