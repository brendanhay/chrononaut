{-# LANGUAGE OverloadedStrings #-}

module Chrononaut.Schema (
    -- * Queries
      tableExists
    , createTable
    , getRevision
    , setRevision

    -- -- * Load/Run Files
    -- , runFile
    ) where

import Chrononaut.Types
import Control.Monad
import Control.Monad.CatchIO
import Data.Int
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField)
import Safe

tableExists :: HasDB m => m Bool
tableExists = fromMaybe False `liftM` maybeQuery sql ()
  where
    sql = "SELECT true FROM pg_tables \
          \WHERE schemaname = 'public' \
          \AND tablename    = 'migration_revision';"

createTable :: HasDB m => m Bool
createTable = (1 ==) `liftM` dbExecute sql ()
  where
    sql = "CREATE TABLE IF NOT EXISTS migration_revision (\
          \revision bigint NOT NULL);"

getRevision :: HasDB m => m (Maybe Int)
getRevision = maybeQuery sql ()
  where
    sql = "SELECT COALESCE (revision, 0) \
          \FROM migration_revision \
          \LIMIT 1;"

setRevision :: HasDB m => Int -> m Bool
setRevision n = (1 ==) `liftM` dbExecute sql (Only n)
  where
    sql = "DELETE FROM migration_revision; \
          \INSERT INTO migration_revision VALUES (?);"

maybeQuery :: (HasDB m, ToRow q, FromField r) => Query -> q -> m (Maybe r)
maybeQuery q ps = do
    rs <- dbQuery q ps
    return $! (\(Only n) -> n) `liftM` headMay rs

dbQuery :: (HasDB m, ToRow q, FromRow r) => Query -> q -> m [r]
dbQuery q ps = dbTransaction (withDB $ \c -> query c q ps)

dbExecute :: (HasDB m, ToRow q) => Query -> q -> m Int64
dbExecute q ps = dbTransaction (withDB $ \c -> execute c q ps)

dbTransaction :: HasDB m => m a -> m a
dbTransaction io = do
    withDB begin
    r <- io `onException` withDB rollback
    withDB commit
    return $! r
