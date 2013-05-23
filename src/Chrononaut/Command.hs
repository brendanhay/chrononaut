module Chrononaut.Command where
    --   initialise
    -- , status
    -- , create
    -- , migrate
    -- , rollback
    -- , redo
    -- , test
    -- ) where

import Prelude hiding (log)

import Chrononaut.Directory
import Chrononaut.Schema
import Chrononaut.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.Monoid

import qualified Chrononaut.Log as Log

-- Switch back to a DATABASE_URL and --database-url

initialise :: FilePath -> [FilePath] -> String -> Bool -> IO ()
initialise root paths url force = runApp root paths url $ do
    md <- migrationDir
    fs <- dataFiles
    liftIO $ createDir md >> copyFiles force fs root

status :: FilePath -> [FilePath] -> String -> Bool -> IO ()
status root paths url force = runApp root paths url $ do
    getRevision >>= liftIO . print
    --Log.getLog

-- create :: FilePath -> String -> IO ()
-- create dir desc = do
--     cfg <- getConfig dir []
--     setup cfg
--     createDir $ migrationDir cfg
--     renderMigrations desc cfg

-- migrate :: FilePath -> Bool -> Int -> Maybe Int -> [FilePath] -> IO ()
-- migrate dir force step rev paths = do
--     cfg <- getConfig dir paths
--     setup cfg
--     log <- Log.getLog cfg
--     either error (mapM_ (run cfg))
--            (Log.diff step rev log)

-- run :: Config -> Log.Rev -> IO ()
-- run cfg rev@(Log.Rev n _) = do
--     !s <- runMigrate cfg $ Log.migratePath rev
--     putStrLn s
--     setRevision n cfg >>= putStrLn

-- rollback :: FilePath -> Bool -> Int -> Maybe Int -> [FilePath] -> IO ()
-- rollback dir force step rev paths = do
--     cfg <- getConfig dir paths
--     setup cfg
--     log <- Log.getLog cfg
--     either error (mapM_ (runRollback cfg . Log.rollbackPath))
--            (Log.diff step rev log)

-- redo :: FilePath -> Bool -> Int -> Maybe Int -> [FilePath] -> IO ()
-- redo dir force step rev paths = do
--     cfg <- getConfig dir paths
--     setup cfg

-- setup :: Config -> IO ()
-- setup cfg = do
--     p <- isRootInitialised cfg
--     unless p $ do
--         putStrLn "Warning: unable to find data files, running init .."
--         initialise (cfgRoot cfg) False

-- test :: FilePath -> [FilePath] -> IO ()
-- test dir paths = return ()
--     -- cfg    <- getConfig dir paths
--     -- (c, _) <- runScript ["string", createVersionTable] cfg
--     -- if c == ExitSuccess
--     --    then putStrLn "Connected successfully." >> exitSuccess
--     --    else putStrLn "Connection failure!" >> exitWith c
