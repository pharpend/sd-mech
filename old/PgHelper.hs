-- |Helper functions for connecting to postgres in the test suite
module PgHelper where

import SdMech.Util

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Database.Persist.Postgresql
import Database.Postgresql.Simple
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Test.Hspec (runIO)
import Test.Hspec.Core.Spec
import Test.QuickCheck

-- |Persistent demands a logger. I don't care, it's not getting a Logger.
type MechM x = SqlPersistT (NoLoggingT IO) x

-- |Connect to a local cluster, run an action, then delete the cluster.
withLocalCluster :: Migration -> -> MechM ()
withLocalCluster action = do
    connStr <- liftIO $ formatPgConnStr <$> localClusterLocation
    testDBName <- liftIO createTempName
    createDB connStr testDBName
    withPostgresqlPool connStr 1 action
    dropDB connStr testDBName
  where
    formatPgConnStr foo =
      "postgresql:///ignored?host=" <+> foo

    createTempName = do
        suffix <- generate . vectorOf 64 . elements $
            ['a' .. 'z'] <+> ['0'..'9']
        return $ "mechtest_" <+> suffix

    appendDBName init nom =
        init <+> "&dbname=" <+> nom

    createDB connString dbnom =
        pgExecute (appendDBName "postgres" connString) $
            "create database"
            <+> read ("\"" <+> dbnom <+> "\"")

    dropDB connString dbnom =
        pgExecute (appendDBName "postgres" connString) $
            "drop database"
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
