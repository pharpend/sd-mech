-- |Types that are built to be stored in a database.
module SdMech.PersistEntities where

import Data.Set (Set)

import SdMech.Funds
import SdMech.Util

share [mkPersist sqlSettings, mkMigrate "migrateMech"]
      [persistLowerCase|
      MPatron
          funds Funds
          externalKey Int
          ExternalPatron externalKey
      MProject
          funds Funds
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
