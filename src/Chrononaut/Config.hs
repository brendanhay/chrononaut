{-# LANGUAGE RecordWildCards #-}

module Chrononaut.Config where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import Paths_chrononaut
import System.Directory
import System.Environment
import System.FilePath

type Env = [(String, String)]

data Config = Config
    { cfgRoot :: FilePath
    , cfgData :: FilePath
    , cfgEnv  :: Env
    }

migrationDir :: Config -> FilePath
migrationDir = (`rootPath` "migrate")

script :: Config -> FilePath
script = (`rootPath` "chrononaut.sh")

migrateTmpl, rollbackTmpl  :: Config -> FilePath
migrateTmpl  = (`rootPath` "migrate.tmpl")
rollbackTmpl = (`rootPath` "rollback.tmpl")

rootPath :: Config -> FilePath -> FilePath
rootPath cfg = joinPath . (cfgRoot cfg :) . replicate 1

dataFiles :: Config -> IO [FilePath]
dataFiles Config{..} = do
    fs <- getDirectoryContents cfgData
    return . map (joinDir cfgData) $ filter (not . isPrefixOf ".") fs

getConfig :: FilePath -> [FilePath] -> IO Config
getConfig root envs = Config root <$> getDataDir <*> loadEnvironment envs

isRootInitialised :: Config -> IO Bool
isRootInitialised cfg = and <$> mapM (doesFileExist . ($ cfg))
    [ script
    , migrateTmpl
    , rollbackTmpl
    , migrationDir
    ]

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
