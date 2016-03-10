-- |Mutations and queries to the database
module SdMech.Combinators where

import SdMech.Funds
import SdMech.Types

import Control.Error
import Control.Lens (view)
import Control.Monad.Except
import qualified Database.Persist as P
import Database.Esqueleto
import Data.Vector (Vector)
import qualified Data.Vector as V

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

-- |Delete a patron
deletePatron :: IsMechPatron a => a -> MechM ()
deletePatron a =
    P.deleteBy $ UniqueMechPatron (toMechPatron a)

-- |Get the pledges associated with a patron.  Will throw 'NoSuchPatron' if
-- the patron doesn't exist.
getPatronPledges :: IsMechPatron r => r -> EMechM (Vector (Entity MechPledge))
getPatronPledges r = do
    Entity patronK _ <- failWithM NoSuchPatron $
        P.getBy $ UniqueMechPatron (toMechPatron r)
    pledges <- right $ select $ from $ \pledge -> do
        where_ (pledge^.MechPledgePatron ==. val patronK)
        return pledge
    return $ V.fromList pledges

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


-- |Delete a project
deleteProject :: IsMechProject a => a -> MechM ()
deleteProject a =
    P.deleteBy $ UniqueMechProject (toMechProject a)

-- |Get the pledges associated with a project.  Will throw 'NoSuchProject' if
-- the project doesn't exist.
getProjectPledges :: IsMechProject r => r -> EMechM (Vector (Entity MechPledge))
getProjectPledges r = do
    Entity projectK _ <- failWithM NoSuchProject $
        P.getBy $ UniqueMechProject (toMechProject r)
    pledges <- right $ select $ from $ \pledge -> do
        where_ (pledge^.MechPledgeProject ==. val projectK)
        return pledge
    return $ V.fromList pledges

--------------------------------------------------------------------------------
-- * Pledges

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
    Entity patronK patronV <- failWithM NoSuchPatron $
        P.getBy $ UniqueMechPatron (toMechPatron a)
    Entity projectK _ <- failWithM NoSuchProject $
        P.getBy $ UniqueMechProject (toMechProject r)
    numberOfPledgesToProject <- V.length <$> (getProjectPledges r)
    if numberOfPledgesToProject > (fundsToInt (Funds 3 `times` (view funds patronV)))
        then throwError InsufficientFunds
        else failWithM ExistentPledge $
            P.insertUnique (MechPledge patronK projectK)
