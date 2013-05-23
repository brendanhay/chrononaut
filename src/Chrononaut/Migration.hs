{-# LANGUAGE DeriveDataTypeable #-}

module Chrononaut.Migration (
    -- * Exported Functions
      renderMigrations
    ) where

import Chrononaut.Config
import Chrononaut.Log
import Control.Applicative
import Control.Monad
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Safe
import System.Directory
import System.FilePath
import Text.Hastache
import Text.Hastache.Context

import qualified Data.ByteString.Lazy.Char8 as BL

data Context = Context
    { name      :: !String
    , timestamp :: !String
    , migrate   :: !String
    , rollback  :: !String
    } deriving (Data, Typeable)

renderMigrations :: String -> Config -> IO ()
renderMigrations desc cfg = do
    rev@(Rev n s) <- newRevision desc

    let up   = migratePath rev
        down = rollbackPath rev
        ctx  = Context s (show n) up down

    zipWithM_ (render cfg ctx) [migrateTmpl, rollbackTmpl] [up, down]

render :: Data a => Config -> a -> (Config -> FilePath) -> FilePath -> IO ()
render cfg ctx f name = do
    bs <- hastacheFile defaultConfig (f cfg) $ mkGenericContext ctx
    putStrLn $ "Writing " <> path <> " ..."
    BL.writeFile path bs
  where
    path = joinPath [migrationDir cfg, name]
