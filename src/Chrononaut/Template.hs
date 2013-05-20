{-# LANGUAGE DeriveDataTypeable #-}

module Chrononaut.Template where

import Chrononaut.Config
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
import System.IO
import System.Locale
import System.Process
import Text.Hastache
import Text.Hastache.Context

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Context = Context
    { description  :: String
    , currentTime  :: String
    , identifier   :: String
    , migrateFile  :: String
    , rollbackFile :: String
    } deriving (Data, Typeable)

renderMigrations :: String -> Config -> IO ()
renderMigrations desc cfg = do
    t   <- getCurrentTime

    let ts   = timeStamp t
        up   = fileName desc "up" ts
        down = fileName desc "down" ts
        ctx  = Context desc (show t) ts up down

    zipWithM_ (render ctx) [migrateTmpl cfg, rollbackTmpl cfg] [up, down]

render :: Data a => a -> FilePath -> FilePath -> IO ()
render ctx tmpl path = do
    bs <- hastacheFile defaultConfig tmpl $ mkGenericContext ctx
    putStrLn $ "Writing " <> path <> " ..."
    BL.writeFile path bs

timeStamp :: UTCTime -> String
timeStamp = take 16 . formatTime defaultTimeLocale "%Y%m%d%M%S%q"

fileName :: String -> String -> String -> String
fileName desc mode ts = take 250 parts <> ".sql"
  where
    parts = ts <> "-" <> mode <> "-" <> underscore desc

underscore :: String -> String
underscore []     = []
underscore (x:xs) = toLower x : go xs
  where
    go []     = []
    go "_"    = "_"
    go (c:cs)
        | isUpper c = '_' : toLower c : go cs
        | c == ' '  = go cs
        | otherwise = c   : go cs
