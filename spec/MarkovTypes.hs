-- |Types for the random testing
module MarkovTypes where

import FundsSpec ()
import SdMech

import Test.QuickCheck

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |Patron
newtype Patr = Patr { patrIdent :: Int }
  deriving (Show, Eq, Ord, Arbitrary)

-- |Project
newtype Prj = Prj { prjIdent :: Int }
  deriving (Show, Eq, Ord, Arbitrary)

-- |Events that can happen
data Event = RunIteration
           | PatrSpawn Patr Funds
           | PatrDie Patr
           | PatrDeposit Patr Funds
           | PatrWithdraw Patr Funds
           | PatrMkPledge Patr Prj
           | PatrActivatePledge Patr Prj
           | PatrSuspendPledge Patr Prj
           | PrjSpawn Prj Funds
           | PrjDie Prj
           | PrjDeposit Prj Funds
           | PrjWithdraw Prj Funds
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance IsMechPatron Patr where
    toMechPatron (Patr i) = i
    fromMechPatron = Patr

instance IsMechProject Prj where
    toMechProject (Prj i) = i
    fromMechProject = Prj

instance Arbitrary Event where
    arbitrary =
        oneof [ pure RunIteration
              , PatrSpawn <$> arbitrary <*> arbitrary
              , PatrDie <$> arbitrary
              , PatrDeposit <$> arbitrary <*> arbitrary
              , PatrWithdraw <$> arbitrary <*> arbitrary
              , PatrMkPledge <$> arbitrary <*> arbitrary
              , PatrActivatePledge <$> arbitrary <*> arbitrary
              , PatrSuspendPledge <$> arbitrary <*> arbitrary
              , PrjSpawn <$> arbitrary <*> arbitrary
              , PrjDie <$> arbitrary
              , PrjDeposit <$> arbitrary <*> arbitrary
              , PrjWithdraw <$> arbitrary <*> arbitrary
              ]

-- ** Lenses and such
makeLensesWith camelCaseFields ''Patr
makeLensesWith camelCaseFields ''Prj
makePrisms ''Event
