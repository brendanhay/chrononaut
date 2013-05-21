{-# LANGUAGE RecordWildCards #-}

module Chrononaut.Command (
      initialise
    , status
    , create
    , migrate
    , rollback
    , redo
    , test
    ) where

import Prelude hiding (log)

import Chrononaut.Config
import Chrononaut.Directory
import Chrononaut.Migration
import Chrononaut.SQL
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import System.Exit
import System.IO

import qualified Chrononaut.Revision as R
import qualified Paths_chrononaut    as P

-- Investigate switching back to a DATABASE_URL since psql supports it

initialise :: FilePath -> Bool -> IO ()
initialise dir force = do
    cfg@Config{..} <- getConfig dir []
    createDir $ migrationDir cfg
    fs <- dataFiles cfg
    mapM_ (\f -> copyAll force f cfgRoot) fs

-- Show current version, which specific file that is,
-- what the step difference is, etc.
status :: FilePath -> [FilePath] -> IO ()
status dir paths = do
    cfg <- getConfig dir paths
    log <- R.getLog cfg
    print log

create :: FilePath -> String -> IO ()
create dir desc = do
    cfg <- getConfig dir []
    setup cfg
    renderMigration desc cfg

migrate :: FilePath
        -> Bool
        -> Maybe Int
        -> Maybe Int
        -> [FilePath]
        -> IO ()
migrate dir force step rev paths = do
    cfg <- getConfig dir paths
    setup cfg
    

rollback :: FilePath
         -> Bool
         -> Maybe Int
         -> Maybe Int
         -> [FilePath]
         -> IO ()
rollback dir force step rev paths = do
    cfg <- getConfig dir paths
    setup cfg

redo :: FilePath
     -> Bool
     -> Maybe Int
     -> Maybe Int
     -> [FilePath]
     -> IO ()
redo dir force step rev paths = do
    cfg <- getConfig dir paths
    setup cfg

setup :: Config -> IO ()
setup cfg = do
    p <- isRootInitialised cfg
    unless p $ do
        putStrLn "Warning: unable to find data files, running init .."
        initialise (cfgRoot cfg) False

test :: FilePath -> [FilePath] -> IO ()
test dir paths = return ()
    -- cfg    <- getConfig dir paths
    -- (c, _) <- runScript ["string", createVersionTable] cfg
    -- if c == ExitSuccess
    --    then putStrLn "Connected successfully." >> exitSuccess
    --    else putStrLn "Connection failure!" >> exitWith c
