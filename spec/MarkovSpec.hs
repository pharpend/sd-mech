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

-- |Translate an 'Event' into an action in the 'EMechM' monad.
runEvent :: Event -> EMechM ()
runEvent = \case
    RunIteration -> do
        return ()

    PatrSpawn patr -> do
        return ()

    PatrDie patr -> do
        return ()

    PatrDeposit patr funds' -> do
        return ()

    PatrWithdraw patr funds' -> do
        return ()

    PatrMkPledge patr prj -> do
        return ()

    PatrRescindPledge patr prj -> do
        return ()

    PatrSuspendPledge patr prj -> do
        return ()

    PrjSpawn prj -> do
        return ()

    PrjDie prj -> do
        return ()

    PrjDeposit prj funds' -> do
        return ()

    PrjWithdraw prj funds' -> do
        return ()

    PrjRescindPledge prj patr -> do
        return ()

    PrjSuspendPledge prj patr -> do
        return ()
