{-# LANGUAGE RecordWildCards #-}

module Chrononaut.Config where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.ByteString.Lazy  (ByteString)
import Data.Char
import Data.Data
import Data.Function
import Data.List
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Data.Word
import Network.URI
import Paths_chrononaut
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Locale

type Env = [(String, String)]

data Config = Config
    { rootDir        :: FilePath
    , dataDir        :: FilePath
    , environment    :: Env
    , migrationDir   :: FilePath
    , setupScript    :: FilePath
    , migrateScript  :: FilePath
    , rollbackScript :: FilePath
    , migrateTmpl    :: FilePath
    , rollbackTmpl   :: FilePath
    }

dataFiles :: Config -> [FilePath]
dataFiles Config{..} = map f
    [ setupScript
    , migrateScript
    , rollbackScript
    , migrateTmpl
    , rollbackTmpl
    ]
  where
    f x = joinPath [dataDir, last $ split '/' x]

getConfig :: FilePath -> [FilePath] -> IO Config
getConfig root envs = do
    d <- getDataDir
    e <- loadEnvironment envs
    return $ Config root d e
        (f "migrate")
        (f "execute.sh")
        (f "migrate.sh")
        (f "rollback.sh")
        (f "migrate.tmpl")
        (f "rollback.tmpl")
  where
    f = joinPath . (root :) . replicate 1

doScriptsExist :: Config -> IO Bool
doScriptsExist cfg = and <$> mapM (doesFileExist . ($ cfg))
    [ setupScript
    , migrateScript
    , rollbackScript
    ]

createVersionTable :: String
createVersionTable =
    "CREATE TABLE IF NOT EXISTS migration_version (version integer NOT NULL);"

currentVersion :: String
currentVersion =
    "SELECT MAX(version) FROM migration_version LIMIT 1;"

localEnvironment :: FilePath
localEnvironment = "./.env"

loadEnvironment :: [FilePath] -> IO Env
loadEnvironment envs = do
    l <- getEnvironment
    p <- doesFileExist localEnvironment
    s <- mapM readFile $ if p then envs ++ [localEnvironment] else envs
    return $! mergeEnvironments (parseEnvironment s) l

parseEnvironment :: [String] -> Env
parseEnvironment = map (second tail . break (== '=')) . lines . concat

mergeEnvironments :: Env -> Env -> Env
mergeEnvironments a b = nubBy ((==) `on` fst) $ a ++ b

joinDir :: FilePath -> FilePath -> FilePath
joinDir pre = joinPath . (pre :) . replicate 1

split :: Char -> String -> [String]
split delim s | [] <- rest = [token]
              | otherwise  = token : split delim (tail rest)
  where
    (token, rest) = span (/= delim) s
