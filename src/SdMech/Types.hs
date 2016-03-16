-- |The fancy types for the mechanism
module SdMech.Types (module SdMech.Types) where

import SdMech.Funds
import SdMech.Types.PledgeStatus as SdMech.Types

import Control.Lens.TH
import Control.Error
import Database.Persist.Sql
import Database.Persist.TH

type MechM = SqlPersistM
type EMechM = ExceptT MechError MechM

runMechM :: MechM x -> ConnectionPool -> IO x
runMechM = runSqlPersistMPool

-- |Convert a 'MechM' value to an 'EMechM' value.
right :: MechM x -> EMechM x
right foo = ExceptT (fmap Right foo)

-- |Convert an 'EMechM' value into a 'MechM' value
coRight :: EMechM x -> MechM (Either MechError x)
coRight = runExceptT

data MechError = ExistentPatron
               | NoSuchPatron
               | ExistentProject
               | NoSuchProject
               | InsufficientFunds
               | ExistentPledge
               | NoSuchPledge
               | IntToFundsConversionError
    deriving (Eq, Show, Read)

derivePersistField "MechError"

share [mkPersist sqlSettings, mkMigrate "migrateMech"]
      [persistLowerCase|
      MechPatron
          funds Funds
          externalKey Int
          ExternalPatron externalKey
          UniqueMechPatron externalKey
          deriving Eq Show
      MechProject
          funds Funds
          externalKey Int
          ExternalProject externalKey
          UniqueMechProject externalKey
          deriving Eq Show
      MechPledge
          patron MechPatronId
          project MechProjectId
          status MechPledgeStatus
          UniqueMechPledge patron project
          deriving Eq Show
      |]

class IsMechPatron x where
  toPatron :: x -> Int
  fromPatron :: Int -> x

class IsMechProject x where
  toProject :: x -> Int
  fromProject :: Int -> x

-- **Lenses

makePrisms ''MechError
makeLensesWith camelCaseFields ''MechPatron
makeLensesWith camelCaseFields ''MechProject
makeLensesWith camelCaseFields ''MechPledge
