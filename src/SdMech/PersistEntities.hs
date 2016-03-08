-- |Types that are built to be stored in a database.
module SdMech.PersistEntities where

import SdMech.Funds
import SdMech.Util

share [mkPersist sqlSettings, mkMigrate "migrateMech"]
      [persistLowerCase|
      MechPatron
          funds Funds
          externalKey Int
          ExternalPatron externalKey
      MechProject
          funds Funds
          externalKey Int
          ExternalProject externalKey
      MechPledge
          patron MechPatronId
          project MechProjectId
          UniqueMPledge patron project
      |]


class IsMechPatron x where
  toMechPatron :: x -> Int
  fromMechPatron :: Int -> x

class IsMechProject x where
  toMechProject :: x -> Int
  fromMechProject :: Int -> x

makeLensesWith camelCaseFields ''MechPatron
makeLensesWith camelCaseFields ''MechProject
makeLensesWith camelCaseFields ''MechPledge
