-- |Random simulation (and testing) of the mechanism
module MarkovSpec where

import FundsSpec ()
import MarkovTypes
import SdMech

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = context "Randomly-generated simulation" $ do
  context "1 year of randomness" $ do
    iterations <- runIO $ generate (generateIterations 12)
    runMechMSpec $
        runSpecs <$> mapM runIteration iterations

runMechMSpec :: MechM Spec -> Spec  
runMechMSpec action = do
  spec <- runIO $ withLocalCluster action
  spec

runIteration :: [Event] -> MechM Spec
runIteration events = do
    eventSpecs <- runSpecs <$> mapM runEvent events
    runitSpec <- runEvent RunIteration
    return (eventSpecs >> runitSpec)

runSpecs :: [Spec] -> Spec
runSpecs [] = return ()
runSpecs (x : xs) = x >> runSpecs xs

-- |Translate an 'Event' into a specification in the 'EMechM' monad.
runEvent :: Event -> MechM Spec
runEvent e = case e of
    RunIteration -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrSpawn patr -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrDie patr -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrDeposit patr funds' -> return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrWithdraw patr funds' -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrMkPledge patr prj -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrRescindPledge patr prj -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PatrSuspendPledge patr prj -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjSpawn prj -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjDie prj -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjDeposit prj funds' -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjWithdraw prj funds' -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjRescindPledge prj patr -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

    PrjSuspendPledge prj patr -> do
        return $ context (show e) $
            specify "there should be a test here" $
                pendingWith "pharpend's laziness"

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

-- |Connect to a local cluster, run an action, then delete the cluster.
withLocalCluster :: MechM x -> IO x
withLocalCluster action = do
    connStr <- formatPgConnStr <$> localClusterLocation
    testDBName <- createTempName
    -- Type tetris here can be confusing. 'runMechM' returns an IO
    -- action. However, withPostgresqlPool demands a Logger, so we're wrapping
    -- all of this in a NoLoggerT. We first have to lift the runMechM into a
    -- NoLoggerT thing, then rip it back down into the real world.
    let createAndDestroy = do
            createDB connStr testDBName
            actionResult <- action
            dropDB connStr testDBName
            return actionResult
    runNoLoggingT $ withPostgresqlPool connStr 1 (liftIO . runMechM createAndDestroy)
  where    
    formatPgConnStr foo =
      "postgresql:///postgres?host=" <+> foo

    createTempName = do
        suffix <- generate . vectorOf 64 . elements $
            ['a' .. 'z'] <+> ['0'..'9']
        return $ "mechtest_" <+> suffix

    appendDBName init nom =
        init <+> "&dbname=" <+> nom

    createDB connString dbnom =
        pgExecute (appendDBName connString "postgres") $
            "create database "
            <+> read ("\"" <+> dbnom <+> "\"")

    dropDB connString dbnom =
        pgExecute (appendDBName connString "postgres") $
            "drop database "
            <+> read ("\"" <+> dbnom <+> "\"")


-- |Location of the local cluster. It checks for the environment variable
-- @SD_MECH_DB@ first; else uses the current directory, concat
-- @/.postgres-work/sockets@.
localClusterLocation :: IO ByteString
localClusterLocation =
    lookupEnv "SD_MECH_DB" >>= \case
        Just var ->
          return $ B8.pack var
        Nothing -> do
            d <- getCurrentDirectory
            return $ B8.pack (d <+> "/.postgres-work/sockets")


-- |Execute a raw query, then close the connection
pgExecute :: ConnectionString
          -> Query
          -> MechM ()
pgExecute connstr query = liftIO $ do
  conn <- connectPostgreSQL connstr
  _ <- execute_ conn query
  close conn
