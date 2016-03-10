-- |Random simulation (and testing) of the mechanism
module MarkovSpec where

import FundsSpec ()
import MarkovTypes
import SdMech

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = context "Randomly-generated simulation" $ do
    return ()

-- |Generate a number of iterations. 'RunIteration' is not manually
-- interspersed.
generateIterations :: Int -> Gen [[Event]]
generateIterations n =
    vectorOf n generateIteration

-- |Generate a number of arbitrary 'Event's to run (critically, not
-- 'RunIteration', though).
generateIteration :: Gen [Event]
generateIteration =
    listOf $ arbitrary `suchThat` (/= RunIteration)

-- |Translate an 'Event' into a specification in the 'EMechM' monad.
runEvent :: Event -> EMechM Spec
runEvent = \case
    RunIteration -> do
        return $ pendingWith "pharpend's laziness"

    PatrSpawn patr -> do
        return $ pendingWith "pharpend's laziness"

    PatrDie patr -> do
        return $ pendingWith "pharpend's laziness"

    PatrDeposit patr funds' -> do
        return $ pendingWith "pharpend's laziness"

    PatrWithdraw patr funds' -> do
        return $ pendingWith "pharpend's laziness"

    PatrMkPledge patr prj -> do
        return $ pendingWith "pharpend's laziness"

    PatrRescindPledge patr prj -> do
        return $ pendingWith "pharpend's laziness"

    PatrSuspendPledge patr prj -> do
        return $ pendingWith "pharpend's laziness"

    PrjSpawn prj -> do
        return $ pendingWith "pharpend's laziness"

    PrjDie prj -> do
        return $ pendingWith "pharpend's laziness"

    PrjDeposit prj funds' -> do
        return $ pendingWith "pharpend's laziness"

    PrjWithdraw prj funds' -> do
        return $ pendingWith "pharpend's laziness"

    PrjRescindPledge prj patr -> do
        return $ pendingWith "pharpend's laziness"

    PrjSuspendPledge prj patr -> do
        return $ pendingWith "pharpend's laziness"
