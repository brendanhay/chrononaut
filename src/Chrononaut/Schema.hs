{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Chrononaut.Schema (
    -- * Queries
      createTable
    , getRevision
    , setRevision

    -- -- * Load/Run Files
    -- , runFile
    ) where

import Chrononaut.TH
import Chrononaut.Types
import Control.Applicative
import Control.Monad.CatchIO
import Data.Int
import Database.PostgreSQL.Simple
import Safe

createTable :: HasDB m => m Bool
createTable = (1 ==) <$> dbExecute sql ()
  where
    sql = [line|
        CREATE TABLE IF NOT EXISTS migration_revision (
            revision bigint NOT NULL
        );
        |]

getRevision :: HasDB m => m (Maybe Int)
getRevision = do
    rs <- dbQuery sql ()
    return $! (\(Only n) -> n) <$> headMay rs
  where
    sql = [line|
        SELECT COALESCE (revision, 0)
        FROM migration_revision
        LIMIT 1;
        |]

setRevision :: HasDB m => Int -> m Bool
setRevision n = (1 ==) <$> dbExecute sql (Only n)
  where
    sql = [line|
        DELETE FROM migration_revision;
        INSERT INTO migration_revision VALUES (?);
        |]

dbQuery :: (HasDB m, ToRow q, FromRow r) => Query -> q -> m [r]
dbQuery q ps = dbTransaction (withDB $ \c -> query c q ps)

dbExecute :: (HasDB m, ToRow q) => Query -> q -> m Int64
dbExecute tmpl qs = dbTransaction (withDB $ \c -> execute c tmpl qs)

dbTransaction :: HasDB m => m a -> m a
dbTransaction io = do
    withDB begin
    r <- io `onException` withDB rollback
    withDB commit
    return $! r
