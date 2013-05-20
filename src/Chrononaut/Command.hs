{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Chrononaut.Command (
      initialise
    , create
    , migrate
    , rollback
    , redo
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Data
import Data.List
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Paths_chrononaut
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Posix.Signals
import System.ShQQ
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

initialise :: FilePath -> Bool -> IO ()
initialise dir force = do
    createDir dir
    createDir $ joinDir dir "migrate"
    src <- getDataDir
    mapM_ (copyFileTo force dir) $ templatePaths src
    putStrLn "Completed."

create :: FilePath -> String -> IO ()
create dir desc = do
    p  <- and <$> mapM doesFileExist inp
    p' <- doesDirectoryExist out

    if p && p'
     then do
         t <- getCurrentTime
         let ts   = timeStamp t
             up   = fileName desc "up" ts
             down = fileName desc "down" ts
             ctx  = Context desc (show t) ts up down
         zipWithM_ (writeLBS out) [up, down] =<< mapM (render ctx) inp
     else do
         putStrLn "Warning: unable to find templates, running init .."
         initialise dir False
         create dir desc
  where
    inp = templatePaths dir
    out = joinDir dir "migrate"

migrate :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
migrate dir conn force step revision = print step

rollback :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
rollback dir conn force step revision = print step

redo :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
redo dir conn force step revision = print step

timeStamp :: UTCTime -> String
timeStamp = take 16 . formatTime defaultTimeLocale "%Y%m%d%M%S%q"

fileName :: String -> String -> String -> String
fileName desc mode ts = take 250 parts <> ".sql"
  where
    parts = ts <> "-" <> mode <> "-" <> underscore desc

render :: Data a => a -> FilePath -> IO BL.ByteString
render ctx tmpl = hastacheFile defaultConfig tmpl $ mkGenericContext ctx

writeLBS :: FilePath -> String -> BL.ByteString -> IO ()
writeLBS dir name bs = do
    putStrLn $ "Writing " <> path <> " ..."
    BL.writeFile path bs
  where
    path = joinDir dir name

templatePaths :: FilePath -> [FilePath]
templatePaths dir = map (joinDir dir) [migrateTmpl, rollbackTmpl]

migrateTmpl :: String
migrateTmpl = "migrate.tmpl"

rollbackTmpl :: String
rollbackTmpl = "rollback.tmpl"

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

joinDir :: FilePath -> FilePath -> FilePath
joinDir pre = joinPath . (pre :) . replicate 1

createDir :: FilePath -> IO ()
createDir dir = do
    p <- doesDirectoryExist dir
    if p
     then putStrLn $ dir <> " already exists, continuing ..."
     else [sh| mkdir -p $dir |] >> putStrLn ("Creating " <> dir <> " ...")

copyFileTo :: Bool -> FilePath -> FilePath -> IO ()
copyFileTo force dir file = do
    p <- doesFileExist target
    c <- if not force && p
          then yesOrNo $ target <> " already exists, overwrite?"
          else return True
    if c
     then do
         putStrLn $ "Writing " <> target <> " ..."
         copyFile file target
     else putStrLn $ "Skipping " <> target <> " ..."
  where
    target = joinPath [dir, dirName file]

dirName :: FilePath -> String
dirName = BS.unpack . snd . BS.breakEnd (== '/') . BS.pack

yesOrNo :: String -> IO Bool
yesOrNo prompt = do
    putStr $ prompt <> " [y/n]: "
    hFlush stdout
    str <- getLine
    case str of
        "y" -> return True
        "n" -> return False
        _   -> do
            putStrLn "Invalid response, please enter 'y' or 'n'."
            yesOrNo prompt
