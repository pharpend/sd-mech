-- |Random simulation (and testing) of the mechanism
module MarkovSpec where

import FundsSpec ()
import MarkovTypes
import SdMech

import Control.Error (failWithM)
import Control.Lens hiding (elements, (<.>), from)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Either
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = context "Randomly-generated simulation" $ do
  context "100 years of chaos" $ do
    iterations <- runIO $ generate (generateIterations 1200)
    let iterLen = length iterations
    runMechMSpec $ runSpecs <$> mapM (uncurry runIteration') (zip [1..iterLen] iterations)

runMechMSpec :: MechM Spec -> Spec  
runMechMSpec action = do
  spec' <- runIO $ withLocalCluster action
  spec'

runIteration' :: Int -> [Event] -> MechM Spec
runIteration' z events = do
    eventSpecs <- runSpecs <$> mapM (runEvent z) events
    runitSpec <- runEvent z RunIteration
    return (eventSpecs >> runitSpec)

runSpecs :: [Spec] -> Spec
runSpecs [] = return ()
runSpecs (x : xs) = x >> runSpecs xs

-- |Translate an 'Event' into a specification in the 'EMechM' monad.
runEvent :: Int -> Event -> MechM Spec
runEvent z e = case e of
    PatrSpawn patr funds' -> do
      -- Check to see if the patron exists
      patronExists <- fmap isRight $ coRight $ selectPatron patr
      -- Insert him into the table
      result <- coRight $ newPatron funds' patr
      -- See how much money he has
      resultFunds <- coRight $ do
          Entity _ val' <- selectPatron patr
          return (view funds val')
      return $ context (show e) $
        if patronExists
          then context "Patron already exists" $
            it "should return Left ExistentPatron" $
              result `shouldBe` Left ExistentPatron
          else context "Patron does not already exist" $ do
            it "should return Right" $
              result `shouldSatisfy` isRight
            he "should have some money in the bank" $
              resultFunds `shouldBe` Right funds'

    PatrDie patr -> do
      -- Check to see if the patron exists before we murder him
      patronExistsBefore <- fmap isRight $ coRight $ selectPatron patr
      -- Kill him
      potentialError <- coRight $ deletePatron patr
      -- Let's see his pledges (should throw error)
      patronPledges <- coRight $ getPatronPledges patr
      -- Check to see if the patron exists after he's dead
      patronExistsAfter <- coRight $ selectPatron patr
      return $ context (show e) $ do
        if patronExistsBefore
          then context "Patron did exist before deletion" $ do
            it "should delete successfully" $ do
              potentialError `shouldSatisfy` isRight
          else context "Patron did *not* exist before deletion" $ do
            it "should throw NoSuchPatron when deleting" $
              potentialError `shouldBe` Left NoSuchPatron
        context "Regardless of his pre-existence" $ do
          he "should no longer exist" $
            patronExistsAfter `shouldBe` Left NoSuchPatron
          it "should throw NoSuchPatron when examining postmortem pledges" $
            patronPledges `shouldBe` Left NoSuchPatron


    PatrDeposit patr funds' -> do
      patronExists <- isRight' $ selectPatron patr
      patrPrevFunds <- coRight $ getPatronFunds patr
      possibleError <- coRight $ patronDeposit patr funds'
      patrPostFunds <- coRight $ getPatronFunds patr
      return $ context (show e) $ do
        if patronExists
          then context "Patron exists" $ do
            he "should have previous funds" $
              patrPrevFunds `shouldSatisfy` isRight
            it "should deposit successfully" $
              possibleError `shouldBe` Right ()
            describe "the new balance" $ do
              it "should exist" $
                patrPostFunds `shouldSatisfy` isRight
              it "should be previousFunds + newFunds" $
                patrPostFunds `shouldBe` over _Right (<+> funds') patrPrevFunds
              it "should be greater than or equal to the previous balance" $ do
                view _Right patrPostFunds `shouldSatisfy` (>= view _Right patrPrevFunds)
          else context "Patron does not exist" $ do
            it "should throw NoSuchPatron when looking up patron funds" $ 
              patrPrevFunds `shouldBe` Left NoSuchPatron
            it "should throw NoSuchPatron when depositing" $ 
              possibleError `shouldBe` Left NoSuchPatron
            it "should throw NoSuchPatron when checking post-deposit funds" $ 
              patrPostFunds `shouldBe` Left NoSuchPatron
              
               

    PatrWithdraw patr amount -> do
      patronExists <- isRight' $ selectPatron patr
      patrPrevFunds <- coRight $ getPatronFunds patr
      withdrawal <- coRight $ patronWithdraw patr amount
      patrPostFunds <- coRight $ getPatronFunds patr
      return $ context (show e) $
        if not patronExists
          then context "No such patron" $
            context "nonexistence tests" $ do
              specify "patrPrevFunds" $ patrPrevFunds `shouldBe` Left NoSuchPatron
              specify "withdrawal" $ withdrawal `shouldBe` Left NoSuchPatron
              specify "patrPostFunds" $ patrPostFunds `shouldBe` Left NoSuchPatron
          else context "Patron exists" $
            if amount < view _Right patrPrevFunds
              then context "Sufficient funds" $ do
                let Right supposedBalance = do
                      Funds prev' <- patrPrevFunds
                      return $ Funds $ prev' - unFunds amount
                specify "Result should be (GoodWithdrawal amount (patrPrevFunds - amount))" $ do
                  withdrawal `shouldBe` Right (GoodWithdrawal amount supposedBalance)
                specify "Balance should be (patrPrevFunds - amount)" $ do
                  patrPostFunds `shouldBe` Right supposedBalance
              else context "Insufficient funds" $ do
                specify "Result should be (FundsEmpty patrPrevFunds)" $
                  withdrawal `shouldBe` over _Right FundsEmpty patrPrevFunds
                specify "Account balance should be zero" $
                  patrPostFunds `shouldBe` Right zero

    PatrMkPledge patr prj -> do
      patronE <- coRight $ selectPatron patr
      projectE <- coRight $ selectProject prj
      let patronExists = isRight patronE
          projectExists = isRight projectE
      pledgeExists <- fmap isRight $ coRight $ selectPledge patr prj
      sufficientFundsB <- coRight $ patronHasSufficientFundsFor patr prj
      pledge' <- coRight $ do
        pid <- insertPledge patr prj
        failWithM NoSuchPledge $ P.get pid
      return $ context (show e) $
        if | not patronExists ->
              context "Patron does not exist" $
                it "should fail with NoSuchPatron" $
                  pledge' `shouldBe` Left NoSuchPatron
           | not projectExists ->
              context "Project does not exist" $
                it "should fail with NoSuchProject" $
                  pledge' `shouldBe` Left NoSuchProject
           | sufficientFundsB == Right False ->
              context "Patron does not have sufficient funds" $
                specify "Pledge should fail with InsufficientFunds" $
                  pledge' `shouldBe` Left InsufficientFunds
           | pledgeExists ->
              context "Pledge already exists" $
                it "should fail with ExistentPledge" $
                  pledge' `shouldBe` Left ExistentPledge
           | otherwise ->
              context "Everything appears okay" $
                specify "pledge should be (Right (Pledge patr prj StActive))" $ do
                  let Right (Entity patrKey _) = patronE
                      Right (Entity prjKey _) = projectE
                  pledge' `shouldBe` Right (MechPledge patrKey prjKey StActive)

    PatrActivatePledge patr prj -> do
      eitherPatron <- coRight $ selectPatron patr
      eitherProject <- coRight $ selectProject prj
      eitherPledge <- coRight $ selectPledge patr prj
      funded <- coRight $ patronHasSufficientFundsFor patr prj
      newStatus <- coRight $ patronActivatePledge patr prj
      newStatus' <- coRight $ getPledgeStatus patr prj
      return $ context (show e) $ do
        context "The new status" $ do
          if | isLeft eitherPatron ->
                context "Patron does not exist" $ do
                  it "should be Left NoSuchPatron" $
                    newStatus `shouldBe` Left NoSuchPatron
             | isLeft eitherProject ->
                context "Project does not exist" $ do
                  it "should be Left NoSuchProject" $
                    newStatus `shouldBe` Left NoSuchProject
             | isLeft eitherPledge ->
                context "Pledge does not exist" $ do
                  it "should be Left NoSuchPledge" $
                    newStatus `shouldBe` Left NoSuchPledge
             | otherwise ->
                context "Everything appears to exist" $
                  if funded == Right True
                    then context "Patron has sufficient funds" $
                      it "should be Right StActive" $
                        newStatus `shouldBe` Right StActive
                    else context "Patron does not have sufficient funds" $
                      it "should be Right StImpoverishedPatron" $
                        newStatus `shouldBe` Right StImpoverishedPatron
          context "Regardless of existence" $
            it "should match a query of the pledge status" $
              newStatus `shouldBe` newStatus'


    PatrSuspendPledge patr prj -> do
      eitherPatron <- coRight $ selectPatron patr
      eitherProject <- coRight $ selectProject prj
      eitherPledge <- coRight $ selectPledge patr prj
      _ <- coRight $ patronSuspendPledge patr prj
      newStatus <- coRight $ getPledgeStatus patr prj
      return $ context (show e) $ do
        context "The new status" $ do
          if | isLeft eitherPatron ->
                context "Patron does not exist" $ do
                  it "should be Left NoSuchPatron" $
                    newStatus `shouldBe` Left NoSuchPatron
             | isLeft eitherProject ->
                context "Project does not exist" $ do
                  it "should be Left NoSuchProject" $
                    newStatus `shouldBe` Left NoSuchProject
             | isLeft eitherPledge ->
                context "Pledge does not exist" $ do
                  it "should be Left NoSuchPledge" $
                    newStatus `shouldBe` Left NoSuchPledge
             | otherwise ->
                context "Everything appears to exist" $
                  it "should be Right StPatronSuspended" $
                    newStatus `shouldBe` Right StPatronSuspended


    PrjSpawn prj funds' -> do
      -- Check to see if the project exists
      projectExists <- fmap isRight $ coRight $ selectProject prj
      -- Insert him into the table
      result <- coRight $ newProject funds' prj
      -- See how much money he has
      resultFunds <- coRight $ do
          Entity _ val' <- selectProject prj
          return (view funds val')
      return $ context (show e) $
        if projectExists
          then context "Project already exists" $
            it "should return Left ExistentProject" $
              result `shouldBe` Left ExistentProject
          else context "Project does not already exist" $ do
            it "should return Right" $
              result `shouldSatisfy` isRight
            he "should have some money in the bank" $
              resultFunds `shouldBe` Right funds'

    PrjDie prj -> do
      -- Check to see if the project exists before we murder him
      projectExistsBefore <- fmap isRight $ coRight $ selectProject prj
      -- Kill him
      potentialError <- coRight $ deleteProject prj
      -- Let's see his pledges (should throw error)
      projectPledges <- coRight $ getProjectPledges prj
      -- Check to see if the project exists after he's dead
      projectExistsAfter <- coRight $ selectProject prj
      return $ context (show e) $ do
        if projectExistsBefore
          then context "Project did exist before deletion" $ do
            it "should delete successfully" $ do
              potentialError `shouldSatisfy` isRight
          else context "Project did *not* exist before deletion" $ do
            it "should throw NoSuchProject when deleting" $
              potentialError `shouldBe` Left NoSuchProject
        context "Regardless of his pre-existence" $ do
          he "should no longer exist" $
            projectExistsAfter `shouldBe` Left NoSuchProject
          it "should throw NoSuchProject when examining postmortem pledges" $
            projectPledges `shouldBe` Left NoSuchProject

    PrjDeposit prj funds' -> do
      projectExists <- isRight' $ selectProject prj
      prjPrevFunds <- coRight $ getProjectFunds prj
      possibleError <- coRight $ projectDeposit prj funds'
      prjPostFunds <- coRight $ getProjectFunds prj
      return $ context (show e) $ do
        if projectExists
          then context "Project exists" $ do
            he "should have previous funds" $
              prjPrevFunds `shouldSatisfy` isRight
            it "should deposit successfully" $
              possibleError `shouldBe` Right ()
            specify "the new balance should exist" $
              prjPostFunds `shouldSatisfy` isRight
            specify "the new balance should be previousFunds + newFunds" $
              prjPostFunds `shouldBe` over _Right (<+> funds') prjPrevFunds
          else context "Project does not exist" $ do
            it "should throw NoSuchProject when looking up project funds" $ 
              prjPrevFunds `shouldBe` Left NoSuchProject
            it "should throw NoSuchProject when depositing" $ 
              possibleError `shouldBe` Left NoSuchProject
            it "should throw NoSuchProject when checking post-deposit funds" $ 
              prjPostFunds `shouldBe` Left NoSuchProject

    PrjWithdraw prj amount -> do
      projectExists <- isRight' $ selectProject prj
      prjPrevFunds <- coRight $ getProjectFunds prj
      withdrawal <- coRight $ projectWithdraw prj amount
      prjPostFunds <- coRight $ getProjectFunds prj
      return $ context (show e) $
        if not projectExists
          then context "No such project" $
            context "nonexistence tests" $ do
              specify "prjPrevFunds" $ prjPrevFunds `shouldBe` Left NoSuchProject
              specify "withdrawal" $ withdrawal `shouldBe` Left NoSuchProject
              specify "prjPostFunds" $ prjPostFunds `shouldBe` Left NoSuchProject
          else context "Project exists" $
            if amount < view _Right prjPrevFunds
              then context "Sufficient funds" $ do
                let Right supposedBalance = do
                      Funds prev' <- prjPrevFunds
                      return $ Funds $ prev' - unFunds amount
                specify "Result should be (GoodWithdrawal amount (prjPrevFunds - amount))" $ do
                  withdrawal `shouldBe` Right (GoodWithdrawal amount supposedBalance)
                specify "Balance should be (prjPrevFunds - amount)" $ do
                  prjPostFunds `shouldBe` Right supposedBalance
              else context "Insufficient funds" $ do
                specify "Result should be (FundsEmpty prjPrevFunds)" $
                  withdrawal `shouldBe` over _Right FundsEmpty prjPrevFunds
                specify "Account balance should be zero" $
                  prjPostFunds `shouldBe` Right zero

    RunIteration -> do
      patrEntsPre <- P.selectList [] [] :: MechM [Entity MechPatron]
      let idToPatrPre = M.fromList $ do
              Entity patrid patron' <- patrEntsPre
              return (patrid, patron')
      patridToPledgesPre <- fmap M.fromList $
        forM patrEntsPre $ \(Entity patrid _) -> do
          patrPledges <- coRight $ getPatronPledges' patrid
          return (patrid, patrPledges)
      let patridToActPledgesPre = M.map (\(Right pledges) ->
                                            V.filter (\(Entity _ p) -> view status p == StActive) pledges)
                                        patridToPledgesPre
      patridToDuesPre <- fmap M.fromList $
        forM patrEntsPre $ \(Entity patrid _) -> do
          Right dues <- coRight $ patronDues' patrid
          return (patrid, dues)
      projectEntitiesBefore <- P.selectList [] [] :: MechM [Entity MechProject]
      let prjidMapToFundsBefore = M.fromList $ do
              Entity prjid project' <- projectEntitiesBefore
              return (prjid, view funds project')

      _ <- coRight $ runIteration

      patrEntsPost <- P.selectList [] [] :: MechM [Entity MechPatron]
      let idToPatrPost = M.fromList $ do
              Entity patrid patron' <- patrEntsPost
              return (patrid, patron')
      patridToPledgesPost <- fmap M.fromList $
        forM patrEntsPost $ \(Entity patrid _) -> do
          patrPledges <- coRight $ getPatronPledges' patrid
          return (patrid, patrPledges)
      pledgesPost <- P.selectList [] [] :: MechM [Entity MechPledge]
      let pledgesPostMap = M.fromList $ fmap (\(Entity k v) -> (k, v)) pledgesPost
      projectEntitiesAfter <- P.selectList [] [] :: MechM [Entity MechProject]
      let prjidMapToFundsAfter = M.fromList $ do
              Entity prjid project' <- projectEntitiesAfter
              return (prjid, view funds project')

      return $ context (show (z, e)) $ do
        context "For each patron" $ do
          forM_ patrEntsPre $ \(Entity patrid _) ->
            context ("Patron number " <+> (show patrid)) $ do
              he "should have less money after than before" $
                shouldSatisfy (view funds (idToPatrPost ! patrid))
                              (<= (view funds (idToPatrPre ! patrid)))
              if view funds (idToPatrPost ! patrid) == zero
                then context "zero funds left" $ do
                  forM_ (patridToPledgesPost ! patrid) $ \pledges ->
                    forM_ pledges $ \(Entity plid pl) ->
                      context ("Pledge with key " <+> show plid) $
                        it "should not be StActive" $ do
                          view status pl `shouldNotBe` StActive
                  let Right prevPledges = patridToPledgesPre ! patrid
                      -- Right postPledges = patridToPledgesPost ! patrid
                      prevActivePledges = V.filter (\(Entity _ x) -> view status x == StActive) prevPledges
                  context "previously active pledges should now have status StImpoverishedPatron" $
                    forM_ prevActivePledges $ \(Entity plid _) -> do
                      specify ("pledge with id " <+> show plid) $
                        view status (pledgesPostMap ! plid) `shouldBe` StImpoverishedPatron
                else context "still has funds" $ do
                  context "should have his previous funds minus what he pledged" $ do
                    let dues = patridToDuesPre ! patrid
                        patrpl = patridToPledgesPre ! patrid
                        patrapl = patridToActPledgesPre ! patrid
                        previousFunds = view funds (idToPatrPre ! patrid)
                        afterFunds = view funds (idToPatrPost ! patrid)
                    context ("pledges are " <+> show (over _Right V.length patrpl)) $ do
                      context ("active pledges are " <+> show (V.length patrapl)) $ do
                        context ("dues are " <+> show dues) $ do
                          context ("previousFunds are " <+> show previousFunds) $ do
                            it ("afterFunds are " <+> show afterFunds) $ do
                              afterFunds `shouldBe` (balanceAfter (withdraw previousFunds dues))
                    
        context "For each project" $ do
          forM_ projectEntitiesBefore $ \(Entity prjid _) -> do
            context ("Project number " <+> show prjid) $ do
              it "should have more money after than before" $
                shouldSatisfy (prjidMapToFundsBefore ! prjid)
                              (<= (prjidMapToFundsAfter ! prjid))
      


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
            runMigration migrateMech
            actionResult <- action
            transactionUndo
            dropDB connStr testDBName
            return actionResult
    runNoLoggingT $ withPostgresqlPool connStr 10 (liftIO . runMechM createAndDestroy)
  where    
    formatPgConnStr foo =
      "postgresql:///postgres?host=" <+> foo

    createTempName = do
        suffix <- generate . vectorOf 16 . elements $
            ['a' .. 'z'] <+> ['0'..'9']
        return $ "mechtest_" <+> suffix

    appendDBName init' nom =
        init' <+> "&dbname=" <+> nom

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
pgExecute connstr query' = liftIO $ do
  conn <- connectPostgreSQL connstr
  _ <- execute_ conn query'
  close conn

-- |We like to be non-inclusive. This is a clone of 'it'
he :: Example a => String -> a -> SpecWith (Arg a)
he = it

-- |We like to be non-inclusive. This is a clone of 'it'
his :: Example a => String -> a -> SpecWith (Arg a)
his = it

-- |Checks if MechM result is successful
isRight' :: forall b. EMechM b -> MechM Bool
isRight' = fmap isRight . coRight
