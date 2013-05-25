{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chrononaut.Application (
      runApp
    , withLog
    , setup
    , ensureSetup
    ) where

import Chrononaut.Environment
import Chrononaut.Log
import Chrononaut.Path
import Chrononaut.Schema
import Chrononaut.Types
import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Paths_chrononaut        (getDataDir)
import System.Directory

runApp :: App IO a -> FilePath -> [FilePath] -> String -> IO a
runApp m root paths url = do
    ensureSetup root
    env <- loadEnvs paths
    app <- newApp root env url
    runReaderT m app

withLog :: MonadCatchIO m => (Log -> App m a) -> App m a
withLog f = do
    _    <- createTable
    cur  <- getRevision
    root <- rootDir
    liftIO (getLog cur root) >>= f

setup :: Bool -> FilePath -> IO ()
setup force root = do
    fs <- getDataFiles
    createDirectoryIfMissing True root
    copyFiles force fs root

ensureSetup :: FilePath -> IO ()
ensureSetup root = do
    p <- isSetup root
    unless p $ do
        putStrLn "Warning: unable to find data files, initialising .."
        setup False root

getDataFiles :: IO [FilePath]
getDataFiles = do
    dir <- getDataDir
    return $! map (`joinPaths` dir) ["migrate.tmpl", "rollback.tmpl"]

isSetup :: FilePath -> IO Bool
isSetup root = and <$>
    mapM (doesFileExist . (`joinPaths` root))
        [ "migrate.tmpl"
        , "rollback.tmpl"
        ]
