{-# LANGUAGE DeriveDataTypeable #-}

module Chrononaut.Command (
      initialise
    , create
    , migrate
    , rollback
    , redo
    , test
    ) where

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

type Env = [(String, String)]

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
    mapM_ (copyFileTo force dir) $ dataFiles src

create :: FilePath -> String -> IO ()
create dir desc = do
    setup dir
    t <- getCurrentTime
    let ts   = timeStamp t
        up   = fileName desc "up" ts
        down = fileName desc "down" ts
        ctx  = Context desc (show t) ts up down
    lbs <- mapM (render ctx) $ templateFiles dir
    zipWithM_ (writeLBS $ joinDir dir "migrate") [up, down] lbs

migrate :: FilePath
        -> String
        -> Bool
        -> Maybe Int
        -> Maybe Int
        -> [FilePath]
        -> IO ()
migrate dir conn force step revision paths = do
    setup dir

rollback :: FilePath
         -> String
         -> Bool
         -> Maybe Int
         -> Maybe Int
         -> [FilePath]
         -> IO ()
rollback dir conn force step revision paths = do
    setup dir

redo :: FilePath
     -> String
     -> Bool
     -> Maybe Int
     -> Maybe Int
     -> [FilePath]
     -> IO ()
redo dir conn force step revision paths = do
    setup dir

setup :: FilePath -> IO ()
setup dir = do
    p  <- and <$> mapM doesFileExist (dataFiles dir)
    p' <- doesDirectoryExist $ joinDir dir "migrate"
    unless (p && p') $ do
        putStrLn "Warning: unable to find data files, running init .."
        initialise dir False

test :: FilePath -> String -> [FilePath] -> IO ()
test dir conn paths = do
    p <- connect dir conn paths
    if p
       then exitSuccess
       else exitFailure

connect :: FilePath -> String -> [FilePath] -> IO Bool
connect dir conn paths = do
    setup dir
    e <- Just <$> environment paths
    p <- runProcess s [] Nothing e Nothing Nothing Nothing
    c <- waitForProcess p
    return $ c == ExitSuccess
  where
    s = joinDir dir "test.sh"

local :: FilePath
local = "./.env"

environment :: [FilePath] -> IO Env
environment paths = do
    env <- getEnvironment
    p   <- doesFileExist local
    es  <- mapM f $ if p then paths ++ [local] else paths
    return $! nubBy ((==) `on` fst) $ g es ++ env
  where
    f x = putStrLn ("Reading " <> x <> " ...") >> readFile x
    g   = map (second tail . break (== '=')) . lines . concat

timeStamp :: UTCTime -> String
timeStamp = take 16 . formatTime defaultTimeLocale "%Y%m%d%M%S%q"

fileName :: String -> String -> String -> String
fileName desc mode ts = take 250 parts <> ".sql"
  where
    parts = ts <> "-" <> mode <> "-" <> underscore desc

render :: Data a => a -> FilePath -> IO ByteString
render ctx tmpl = hastacheFile defaultConfig tmpl $ mkGenericContext ctx

writeLBS :: FilePath -> String -> ByteString -> IO ()
writeLBS dir name bs = do
    putStrLn $ "Writing " <> path <> " ..."
    BL.writeFile path bs
  where
    path = joinDir dir name

dataFiles :: FilePath -> [FilePath]
dataFiles dir = templateFiles dir ++ map (joinDir dir)
    [ "migrate.sh"
    , "rollback.sh"
    , "test.sh"
    ]

templateFiles :: FilePath -> [FilePath]
templateFiles dir = map (joinDir dir)
    [ "migrate.tmpl"
    , "rollback.tmpl"
    ]

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
     else do
         putStrLn ("Creating " <> dir <> " ...")
         createDirectoryIfMissing True dir

copyFileTo :: Bool -> FilePath -> FilePath -> IO ()
copyFileTo force dir file = do
    p <- doesFileExist target
    c <- if not force && p
          then yesOrNo $ target <> " already exists, overwrite?"
          else return True
    if c
     then putStrLn ("Writing " <> target <> " ...") >> copyFile file target
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
