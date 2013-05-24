{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chrononaut.Types where

import Control.Applicative
import Control.Arrow
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString            (ByteString)
import Data.Monoid
import Data.Pool
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Network.URI                (isAbsoluteURI)
import Paths_chrononaut           (getDataDir)
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified System.Process        as P

type Env = [(String, String)]

newtype Url = Url ByteString deriving (Show)

newtype DB = DB { getDBPool :: Pool Connection }

class (Applicative m, MonadIO m, MonadCatchIO m) => HasDB m where
    withDB :: (Connection -> IO a) -> m a

data Chrono = Chrono
    { chronoRoot  :: !FilePath
    , chronoShare :: !FilePath
    , chronoDB    :: !DB
    }

type App = StateT Chrono

instance (Functor m, MonadCatchIO m) => HasDB (App m) where
    withDB f = get >>= liftIO . (`withResource` f) . getDBPool . chronoDB

runApp :: FilePath -> [FilePath] -> String -> App IO a -> IO a
runApp root paths murl m = do
    env <- loadEnvs paths
    let !url = parseUrl murl env
    ss  <- Chrono root <$> getDataDir <*> connect url
    evalStateT m ss

connect :: Url -> IO DB
connect (Url bs) = DB <$> createPool (connectPostgreSQL bs) close 1 1 1

rootDir :: Monad m => App m FilePath
rootDir = chronoRoot `liftM` get

rootPath :: Monad m => FilePath -> App m FilePath
rootPath path = joinPaths path `liftM` rootDir

shareDir :: Monad m => App m FilePath
shareDir = chronoShare `liftM` get

sharePath :: Monad m => FilePath -> App m FilePath
sharePath path = joinPaths path `liftM` shareDir

migrationDir :: Monad m => App m FilePath
migrationDir = rootPath "migrate"

templatePath :: Monad m => String -> App m FilePath
templatePath name = rootPath (name ++ ".tmpl")

joinPaths :: FilePath -> FilePath -> FilePath
joinPaths path = joinPath . (: [path])

dataFiles :: MonadIO m => App m [FilePath]
dataFiles = mapM templatePath ["migrate", "rollback"]

isInitialised :: MonadIO m => App m Bool
isInitialised = do
    fs <- dataFiles
    ps <- liftIO $ mapM doesFileExist fs
    return $! and ps

parseUrl :: String -> Env -> Url
parseUrl s env
    | isAbsoluteURI s = f s
    | otherwise =
        maybe (error $ "failed to read environment variable '" <> s <> "'")
              f (s `lookup` env)
  where
    f = Url . BS.pack

localEnv :: FilePath
localEnv = "./.env"

loadEnvs :: [FilePath] -> IO Env
loadEnvs paths = do
    p <- doesFileExist localEnv
    s <- mapM readFile $ if p then paths ++ [localEnv] else paths
    expandVars s

expandVars :: [String] -> IO Env
expandVars vars = do
    (Nothing, Just h, Nothing, p) <- P.createProcess $ expandEnv vars
    !c <- P.waitForProcess p
    !s <- hGetContents h
    hClose h
    when (c /= ExitSuccess)
         (error "Failed to get environment variables")
    return $! parseEnv s

expandEnv :: [String] -> P.CreateProcess
expandEnv vars = (P.shell $ replaceBreaks (concat vars) ++ "env")
    { P.std_out = P.CreatePipe
    }

parseEnv :: String -> Env
parseEnv = map (second tail . break (== '=')) . lines

replaceBreaks :: String -> String
replaceBreaks = map f
  where
    f '\n' = ' '
    f c    = c
