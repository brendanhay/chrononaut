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
import Data.Function
import Data.List
import Data.Monoid
import Data.Pool
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Network.URI                (isAbsoluteURI)
import Paths_chrononaut           (getDataDir)
import System.Directory
import System.Environment
import System.FilePath

import qualified Data.ByteString.Char8 as BS

newtype DB = DB { getDBPool :: Pool Connection }

class (Applicative m, MonadIO m, MonadCatchIO m) => HasDB m where
    withDB :: (Connection -> IO a) -> m a

newtype Url = Url ByteString deriving (Show)

parseUrl :: String -> Env -> Url
parseUrl s env
    | isAbsoluteURI s = f s
    | otherwise =
        maybe (error $ "failed to read environment variable '" <> s <> "'")
              f (s `lookup` env)
  where
    f = Url . BS.pack

connect :: Url -> IO DB
connect (Url bs) = DB <$> createPool (connectPostgreSQL bs) close 1 1 1

data Chrono = Chrono
    { chronoRoot  :: !FilePath
    , chronoShare :: !FilePath
    , chronoDB    :: !DB
    }

type App = StateT Chrono

instance (Applicative m, MonadCatchIO m) => HasDB (App m) where
    withDB f = get >>= liftIO . (`withResource` f) . getDBPool . chronoDB

runApp :: FilePath -> [FilePath] -> String -> App IO a -> IO a
runApp root paths murl m = do
    env <- loadEnvs paths
    let !url = parseUrl murl env
    print url
    ss  <- Chrono root <$> getDataDir <*> connect url
    evalStateT m ss

rootDir :: (Applicative m, Monad m) => App m FilePath
rootDir = chronoRoot <$> get

rootPath :: (Applicative m, Monad m) => FilePath -> App m FilePath
rootPath path = joinPaths path <$> rootDir

shareDir :: (Applicative m, Monad m) => App m FilePath
shareDir = chronoShare <$> get

sharePath :: (Applicative m, Monad m) => FilePath -> App m FilePath
sharePath path = joinPaths path <$> shareDir

migrationDir :: (Applicative m, Monad m) => App m FilePath
migrationDir = rootPath "migrate"

joinPaths :: FilePath -> FilePath -> FilePath
joinPaths path = joinPath . (: [path])

data Template = Migrate | Rollback

instance Show Template where
    show Migrate  = "migrate.tmpl"
    show Rollback = "rollback.tmpl"

dataFiles :: (Applicative m, MonadIO m) => App m [FilePath]
dataFiles = mapM (sharePath . show) [Migrate, Rollback]

isInitialised :: (Applicative m, MonadIO m) => App m Bool
isInitialised = do
    fs <- dataFiles
    ps <- liftIO $ mapM doesFileExist fs
    return $! and ps

type Env = [(String, String)]

localEnv :: FilePath
localEnv = "./.env"

loadEnvs :: [FilePath] -> IO Env
loadEnvs paths = do
    l <- getEnvironment
    p <- doesFileExist localEnv
    s <- mapM readFile $ if p then paths ++ [localEnv] else paths
    return $! parseEnv s `mergeEnvs` l

parseEnv :: [String] -> Env
parseEnv = map (second tail . break (== '=')) . lines . concat

mergeEnvs :: Env -> Env -> Env
mergeEnvs a b = nubBy ((==) `on` fst) $ a ++ b
