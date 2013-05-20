{-# LANGUAGE RecordWildCards #-}

module Chrononaut.Command (
      initialise
    , status
    , create
    , migrate
    , rollback
    , redo
    , test
    ) where

import Chrononaut.Config
import Chrononaut.Template
import Control.Applicative
import Control.Arrow
import Control.Exception    (evaluate)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
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
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Process

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

initialise :: FilePath -> Bool -> IO ()
initialise dir force = do
    c@Config{..} <- getConfig dir []
    mapM_ createDir [rootDir, migrationDir]
    mapM_ (copyFile' force rootDir) $ dataFiles c

-- Show current version, which specific file that is,
-- what the step difference is, etc.
status :: FilePath -> [FilePath] -> IO ()
status dir paths = do
    c@Config{..} <- getConfig dir paths
    (code, res)  <- runScript setupScript [currentVersion] environment
    print res
    exitWith code

create :: FilePath -> String -> IO ()
create dir desc = do
    cfg <- getConfig dir []
    setup cfg
    renderMigrations desc cfg

migrate :: FilePath
        -> Bool
        -> Maybe Int
        -> Maybe Int
        -> [FilePath]
        -> IO ()
migrate dir force step revision paths = do
    cfg <- getConfig dir paths
    setup cfg

rollback :: FilePath
         -> Bool
         -> Maybe Int
         -> Maybe Int
         -> [FilePath]
         -> IO ()
rollback dir force step revision paths = do
    cfg <- getConfig dir paths
    setup cfg

redo :: FilePath
     -> Bool
     -> Maybe Int
     -> Maybe Int
     -> [FilePath]
     -> IO ()
redo dir force step revision paths = do
    cfg <- getConfig dir paths
    setup cfg

setup cfg = do
    p <- doScriptsExist cfg
    unless p $ do
        putStrLn "Warning: unable to find data files, running init .."
        initialise (rootDir cfg) False

test :: FilePath -> [FilePath] -> IO ()
test dir paths = do
    Config{..} <- getConfig dir paths
    (c, _)     <- runScript setupScript [createVersionTable] environment
    if c == ExitSuccess
       then putStrLn "Connected successfully." >> exitSuccess
       else putStrLn "Connection failure!" >> exitWith c

runScript :: FilePath -> [String] -> Env -> IO (ExitCode, String)
runScript script args env = do
    (Nothing, Just out, Nothing, pid) <- createProcess $ (proc script args)
        { std_out = CreatePipe
        , env     = Just env
        }
    res  <- hGetContents out
    evaluate res >> hClose out
    code <- waitForProcess pid
    return (code, res)

createDir :: FilePath -> IO ()
createDir dir = do
    p <- doesDirectoryExist dir
    if p
     then putStrLn $ dir <> " already exists, continuing ..."
     else do
         putStrLn ("Creating " <> dir <> " ...")
         createDirectoryIfMissing True dir

copyFile' :: Bool -> FilePath -> FilePath -> IO ()
copyFile' force dir file = do
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
