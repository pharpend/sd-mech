-- |The fancy types for the mechanism
module SdMech.Types (module SdMech.Types) where

import SdMech.Funds
import SdMech.Util
import SdMech.Types.MechPledgeStatus as SdMech.Types

import Control.Error
import Data.Ord (comparing)
import Database.Persist.Sql

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
               | NoSuchPatron
               | ExistentProject
               | NoSuchProject
               | InsufficientFunds
               | ExistentPledge
               | NoSuchPledge
               | IntToFundsConversionError
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
          UniqueMechPledge patron project
          deriving Eq Show
      |]


instance Eq MechPatron where
    m1 == m2 = mechPatronExternalKey m1 == mechPatronExternalKey m2

instance Ord MechPatron where
    compare = comparing mechPatronExternalKey

instance Eq MechProject where
    m1 == m2 = mechProjectExternalKey m1 == mechProjectExternalKey m2

instance Ord MechProject where
    compare = comparing mechProjectExternalKey

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

