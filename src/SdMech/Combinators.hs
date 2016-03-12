-- |Mutations and queries to the database
module SdMech.Combinators where

import SdMech.Funds
import SdMech.Util ((<+>), zero)
import SdMech.Types

import Control.Error
import qualified Control.Lens as L
import Control.Monad.Except
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Esqueleto hiding ((=.))
import Database.Persist ((=.))
import qualified Database.Persist as P

--------------------------------------------------------------------------------
-- * Patrons

-- |Select a patron
selectPatron :: IsMechPatron a => a -> EMechM (Entity MechPatron)
selectPatron a =
    failWithM NoSuchPatron $
      P.getBy (UniqueMechPatron (toMechPatron a))

-- |Insert a new patron
newPatron :: IsMechPatron a => Funds -> a -> EMechM (Key MechPatron)
newPatron funds' a =
    failWithM ExistentPatron $
        P.insertUnique (MechPatron funds' (toMechPatron a))

-- |Delete a patron. (Also deletes pledges associated with this patron)
deletePatron :: IsMechPatron a => a -> EMechM ()
deletePatron a = do
    -- Delete his pledges first
    pledges' <- getPatronPledges a
    right $ do
      forM_ pledges' $ \(Entity k _) -> P.delete k
      P.deleteBy $ UniqueMechPatron (toMechPatron a)

-- |Get the pledges associated with a patron.  Will throw 'NoSuchPatron' if
-- the patron doesn't exist.
getPatronPledges :: IsMechPatron r => r -> EMechM (Vector (Entity MechPledge))
getPatronPledges r = do
    Entity patronK _ <- selectPatron r
    getPatronPledges' patronK

getPatronPledges' :: Key MechPatron -> EMechM (Vector (Entity MechPledge))
getPatronPledges' patronK = do
    pledges <- right $ select $ from $ \pledge -> do
        where_ (pledge^.MechPledgePatron ==. val patronK)
        return pledge
    return $ V.fromList pledges

-- |How much the patron owes on the next iteration.
patronDues' :: Key MechPatron -> EMechM Funds
patronDues' patronK = do
    patronPledges <- getPatronPledges' patronK
    pledgeValues <- forM patronPledges $ \(Entity _ pl) -> do
        naptp <- numberOfActivePledgesToProject' (L.view project pl)
        failWith IntToFundsConversionError $ intToFunds naptp
    return $ foldl mappend zero pledgeValues

-- |See if the patron has enough funds to pledge to project.
-- 
-- Will throw error if 
-- 
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
patronHasSufficientFundsFor :: (IsMechPatron a, IsMechProject r)
                            => a -> r -> EMechM Bool
patronHasSufficientFundsFor patr prj = do
    Entity patrid _ <- selectPatron patr
    Entity prjid _ <- selectProject prj
    patronHasSufficientFundsFor' patrid prjid

patronHasSufficientFundsFor' :: Key MechPatron -> Key MechProject -> EMechM Bool
patronHasSufficientFundsFor' patrid prjid = do
    fn <- fundsNeededForProject' prjid
    patron' <- failWithM NoSuchPatron $ P.get patrid
    return $ (L.view funds patron') >= fn

-- |How much money patron has
patronFunds :: IsMechPatron a => a -> EMechM Funds
patronFunds patr = do
    Entity _ patr' <- selectPatron patr
    return $ L.view funds patr'

-- |Deposit funds into patron's account
--
-- Will throw error if patron doesn't exist
patronDeposit :: IsMechPatron a => a -> Funds -> EMechM ()
patronDeposit patr funds' = do
    Entity patrid patron' <- selectPatron patr
    right $ P.replace patrid (L.over funds (<+> funds') patron')

-- |Withdraw funds from patron's account. Returns the 'Withdrawal'.
--
-- Will throw error if patron doesn't exist. If this drains the patron's
-- account, then it will set the status of all of the patron's pledges to
-- 'StImpoverishedPatron'.
patronWithdraw :: IsMechPatron a => a -> Funds -> EMechM Withdrawal
patronWithdraw patr amount = do
    Entity patrid _ <- selectPatron patr
    patronWithdraw' patrid amount

patronWithdraw' :: Key MechPatron -> Funds -> EMechM Withdrawal
patronWithdraw' patrid  amount = do
    patron' <- failWithM NoSuchPatron $ P.get patrid
    let withdrawal = withdraw (L.view funds patron') amount
        newFunds = balanceAfter withdrawal
    right $ P.replace patrid $ L.set funds newFunds patron'
    return withdrawal

-- |Attempt to activate pledge. Throws errors if
-- 
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Pledge does not exist ('NoSuchPledge')
-- 
-- This does not throw any errors if the pledge is already active, or if the
-- patron does not have funds. It instead returns the 'MechPledgeStatus'.
-- 
-- Note that
-- 
-- > (patron has funds) -> StActive
-- > (patron does not have funds) -> StImpoverishedPatron
-- 
-- Meaning there is no situation in which this returns 'StPatronSuspended'.
-- 
-- This will mutate the pledge status in the database.
patronActivatePledge :: (IsMechPatron a, IsMechProject r)
                     => a -> r -> EMechM MechPledgeStatus
patronActivatePledge patr prj = do
  Entity pledgeKey pledge' <- selectPledge patr prj
  patronHasFunds <- patronHasSufficientFundsFor patr prj
  if patronHasFunds
    then do
      right $ P.replace pledgeKey $ L.set status StActive pledge'
      return StActive
    else do
      right $ P.replace pledgeKey $ L.set status StImpoverishedPatron pledge'
      return StImpoverishedPatron
      
    
-- |Suspend a pledge. Throws errors if
-- 
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Pledge does not exist ('NoSuchPledge')
-- 
-- Note that this does not take into account what the current status is.
patronSuspendPledge :: (IsMechPatron a, IsMechProject r)
                    => a -> r -> EMechM ()
patronSuspendPledge patr prj = do
  Entity pledgeKey pledge' <- selectPledge patr prj
  right $ P.replace pledgeKey $ L.set status StPatronSuspended pledge'


--------------------------------------------------------------------------------
-- * Projects

-- |Select a project
selectProject :: IsMechProject a => a -> EMechM (Entity MechProject)
selectProject a =
    failWithM NoSuchProject $
      P.getBy (UniqueMechProject (toMechProject a))

-- |Insert a new project
newProject :: IsMechProject a => Funds -> a -> EMechM (Key MechProject)
newProject funds' a =
    failWithM ExistentProject $
        P.insertUnique (MechProject funds' (toMechProject a))


-- |Delete a project. Also deletes pledges associated with project.
deleteProject :: IsMechProject a => a -> EMechM ()
deleteProject prj = do
    -- Delete the pledges first
    pledges' <- getProjectPledges prj
    right $ do
      forM_ pledges' $ \(Entity k _) -> P.delete k
      P.deleteBy $ UniqueMechProject (toMechProject prj)

-- |Get the pledges associated with a project.  Will throw 'NoSuchProject' if
-- the project doesn't exist.
getProjectPledges :: IsMechProject r => r -> EMechM (Vector (Entity MechPledge))
getProjectPledges r = do
    Entity k _ <- selectProject r
    getProjectPledges' k

getProjectPledges' :: Key MechProject -> EMechM (Vector (Entity MechPledge))
getProjectPledges' k = do
    pledges <- right $ select $ from $ \pledge -> do
        where_ (pledge^.MechPledgeProject ==. val k)
        return pledge
    return $ V.fromList pledges

-- |Get the number of pledges of the project
numberOfPledgesToProject :: IsMechProject r => r -> EMechM Int
numberOfPledgesToProject = fmap V.length . getProjectPledges

numberOfPledgesToProject' :: Key MechProject -> EMechM Int
numberOfPledgesToProject' = fmap V.length . getProjectPledges'

-- |Get the number of pledges of the project
numberOfActivePledgesToProject :: IsMechProject p => p -> EMechM Int
numberOfActivePledgesToProject prj = do
    Entity prjid _ <- selectProject prj
    numberOfActivePledgesToProject' prjid

numberOfActivePledgesToProject' :: Key MechProject -> EMechM Int
numberOfActivePledgesToProject' prjid = do
    pledges <- getProjectPledges' prjid
    let pledges' = V.filter (\(Entity _ pledge') -> L.view status pledge' == StActive) pledges
    return $ V.length pledges'

-- |3 times the 'numberOfActivePledgesToProject'
fundsNeededForProject :: IsMechProject r => r -> EMechM Funds
fundsNeededForProject prj = do
    Entity k _ <- selectProject prj
    fundsNeededForProject' k
    
fundsNeededForProject' :: Key MechProject -> EMechM Funds
fundsNeededForProject' k = do
    monthlyIncome <-
        fmap intToFunds (numberOfActivePledgesToProject' k) >>= \case
            Nothing -> throwError IntToFundsConversionError
            Just x -> return x
    return $ Funds 3 <.> monthlyIncome

-- |How much money project has
projectFunds :: IsMechProject a => a -> EMechM Funds
projectFunds prj = do
    Entity _ prj' <- selectProject prj
    return $ L.view funds prj'

-- |Deposit funds into project's account
--
-- Will throw error if project doesn't exist
projectDeposit :: IsMechProject a => a -> Funds -> EMechM ()
projectDeposit prj funds' = do
    Entity prjid project' <- selectProject prj
    right $ P.replace prjid (L.over funds (<+> funds') project')

-- |Withdraw funds from project's account. Returns the 'Withdrawal'.
--
-- Will throw error if project doesn't exist
projectWithdraw :: IsMechProject a => a -> Funds -> EMechM Withdrawal
projectWithdraw prj amount = do
    Entity prjid project' <- selectProject prj
    let withdrawal = withdraw (L.view funds project') amount
        newFunds = balanceAfter withdrawal
    right $ P.replace prjid $ L.set funds newFunds project'
    return withdrawal


--------------------------------------------------------------------------------
-- * Pledges

-- |Select a pledge
--
-- Will throw errors if
--
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Pledge does not exist ('NoSuchPledge')
selectPledge :: (IsMechPatron a, IsMechProject r)
             => a -> r -> EMechM (Entity MechPledge)
selectPledge a r = do
    Entity patronK _ <- selectPatron a
    Entity projectK _ <- selectProject r
    failWithM NoSuchPledge $ P.getBy $
        UniqueMechPledge patronK projectK

-- |Insert a pledge
--
-- Will throw errors if
--
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Patron does not have sufficient funds ('InsufficientFunds'). Patron must
--   have 3 iterations worth of funds.
-- - Pledge already exists ('ExistentPledge')
insertPledge :: (IsMechPatron a, IsMechProject r)
             => a -> r -> EMechM (Key MechPledge)
insertPledge a r = do
    Entity patronK _ <- selectPatron a
    Entity projectK _ <- selectProject r
    patronHasFunds <- patronHasSufficientFundsFor a r
    if not patronHasFunds
        then throwError InsufficientFunds
        else failWithM ExistentPledge $
            P.insertUnique (MechPledge patronK projectK StActive)

-- |Get the status of a pledge. Will throw an error if
-- 
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Pledge does not exists ('NoSuchPledge')
getPledgeStatus :: (IsMechPatron a, IsMechProject r)
                => a -> r -> EMechM MechPledgeStatus
getPledgeStatus a r = fmap (mechPledgeStatus . entityVal) $ selectPledge a r


-- |'Verify' the status. In essence:
-- 
-- - If the status is 'StPatronSuspended', leave it - If the status is
-- 'StImpoverishedPatron', leave it - If the status is 'StActive', make sure the
-- patron has funds, and if not, switch to 'StImpoverishedPatron'
--
-- Will throw error if
-- 
-- - Patron does not exist ('NoSuchPatron')
-- - Project does not exist ('NoSuchProject')
-- - Pledge does not exist ('NoSuchPledge')
verifyPledgeStatus :: (IsMechPatron a, IsMechProject r)
                   => a -> r -> EMechM MechPledgeStatus
verifyPledgeStatus a r =
    (verifyPledgeStatus' . entityKey) =<< selectPledge a r
                  

-- |Similar to 'verifyPledgeStatus', except it takes a 'Key' rather than the
-- associated parties.
verifyPledgeStatus' :: Key MechPledge -> EMechM MechPledgeStatus
verifyPledgeStatus' pledgeId' = do
    MechPledge patronId' projectId' status' <-
        failWithM NoSuchPledge $ P.get pledgeId'
    case status' of
        StActive -> do
            patrFunded <- patronHasSufficientFundsFor' patronId' projectId'
            if not patrFunded
                then do
                    right $ P.update pledgeId' [MechPledgeStatus =. StImpoverishedPatron]
                    return StImpoverishedPatron
                else return StActive
        x -> return x

-- |Run the pledges, so to speak. Take all of the active pledges, and move the
-- money from patrons to projects.
-- 
-- It shouldn't throw any errors, hence the MechM monad
runIteration :: MechM ()
runIteration = do
    pledges <- select $ from $
        \pledge' ->
            do where_ ((pledge' ^. MechPledgeStatus) ==. val StActive)
               return pledge'
    mapM_ (coRight . runPledge) pledges
  where    
    runPledge :: Entity MechPledge -> EMechM ()
    runPledge (Entity pldgid pledge') = verifyPledgeStatus' pldgid >>= \case
        StActive -> do
            let prjid = L.view project pledge'
                patrid = L.view patron pledge'
            withdrawalAmount <- fundsNeededForProject' prjid
            withdrawal <- patronWithdraw' patrid withdrawalAmount
            let amountWithdrawn = case withdrawal of
                    GoodWithdrawal amt _ -> amt
                    FundsEmpty amt -> amt
            project' <- failWithM NoSuchProject $ P.get prjid
            right $ P.replace prjid (L.over funds (<+> amountWithdrawn) project')

        -- If the pledge isn't active for whatever reason, move on
        _ -> return ()
