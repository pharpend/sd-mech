-- |Runs the tests
module Main where

import SdMech
import Test.Tasty.Persist.Postgres

import qualified Data.ByteString.Char8 as B8
import Database.Persist.Postgresql
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Test.Tasty

main :: IO ()
main = do
    lcs <- localConnectionString
    defaultMain (tests lcs)

tests :: ConnectionString -> TestTree
tests connStr =
    withDB (PostgresConf connStr 1) migrateMech $ dbTestGroup "input processor"
        [ dbTestCase "nobody is ever overspent" pending
        , dbTestCase "during payout, projects are independent" pending
        , dbTestCase "patron must support 3 months of pledging" pending
        ]

-- |Connection string to clocal cluster
localConnectionString :: IO ConnectionString
localConnectionString = do
    lcl <- localClusterLocation                      
    return . B8.pack $ "postgresql:///ignored?host=" <+> lcl
    
-- |Location of the local cluster. It checks for the environment variable
-- @SD_MECH_DB@ first; else uses the current directory, concat
-- @/.postgres-work/sockets@.
localClusterLocation :: IO FilePath
localClusterLocation = lookupEnv "SD_MECH_DB" >>= \case
    Just envVal -> return envVal
    Nothing -> do
        cwd <- getCurrentDirectory
        return $ cwd <+> "/.postgres-work/sockets"
