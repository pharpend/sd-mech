-- |The fancy types for the mechanism
module SdMech.Types where

import Control.Error
import Database.Persist.Sql
import SdMech.Funds
import SdMech.Util

type EMechM = ExceptT MechError SqlPersistM
type MechM = SqlPersistM

runMechM :: MechM x -> ConnectionPool -> IO x
runMechM = runSqlPersistMPool

-- |Convert a 'MechM' value to an 'EMechM' value.
right :: MechM x -> EMechM x
right foo = ExceptT (fmap Right foo)

-- |Convert an 'EMechM' value into a 'MechM' value
coRight :: EMechM x -> MechM (Either MechError x)
coRight = runExceptT

data MechError = ExistentPatron
               | ExistentProject
               | NoSuchPatron
               | NoSuchProject
               | InsufficientFunds
               | ExistentPledge
    deriving (Eq, Show)

share [mkPersist sqlSettings, mkMigrate "migrateMech"]
      [persistLowerCase|
      MechPatron
          funds Funds
          externalKey Int
          ExternalPatron externalKey
          UniqueMechPatron externalKey
          deriving Show
      MechProject
          funds Funds
          externalKey Int
          ExternalProject externalKey
          UniqueMechProject externalKey
          deriving Show
      MechPledge
          patron MechPatronId
          project MechProjectId
          UniqueMPledge patron project
          deriving Show
      |]


class IsMechPatron x where
  toMechPatron :: x -> Int
  fromMechPatron :: Int -> x

class IsMechProject x where
  toMechProject :: x -> Int
  fromMechProject :: Int -> x

-- **Lenses

makePrisms ''MechError
makeLensesWith camelCaseFields ''MechPatron
makeLensesWith camelCaseFields ''MechProject
makeLensesWith camelCaseFields ''MechPledge

