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

initialise :: FilePath -> String -> Bool -> IO ()
initialise target conn force = do
    createDir target
    createDir $ joinDir target "migrate"
    src <- getDataDir
    mapM_ (copyFileTo force target) $ templatePaths src
    putStrLn "Completed."

data Context = Context
    { description  :: String
    , currentTime  :: String
    , identifier   :: String
    , migrateFile  :: String
    , rollbackFile :: String
    } deriving (Data, Typeable)

create :: FilePath -> String -> IO ()
create dir desc = do
    p <- and <$> mapM doesFileExist paths
    if p
     then do
         t <- getCurrentTime

         let safe = underscore desc
             ts   = take 16 $ formatTime defaultTimeLocale "%Y%m%d%M%S%q" t
             up   = ts <> "-up-"   <> safe <> ".sql"
             down = ts <> "-down-" <> safe <> ".sql"

         lbs <- mapM (render $ Context desc (show t) ts up down) paths

         print lbs

     else error "Unable to find templatePaths, have you run init?"
  where
    paths = templatePaths dir
    ctx "name"   = MuVariable "Haskell"
    ctx "unread" = MuVariable (100 :: Int)

render ctx path = hastacheFile defaultConfig path $ mkGenericContext ctx

migrate :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
migrate dir conn force step revision = print step

rollback :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
rollback dir conn force step revision = print step

redo :: FilePath -> String -> Bool -> Maybe Int -> Maybe Int -> IO ()
redo dir conn force step revision = print step

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
    c <- if (not force) && p
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
