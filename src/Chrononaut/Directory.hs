module Chrononaut.Directory where

import Control.Monad
import Data.Monoid
import System.Directory hiding (copyFile)
import System.FilePath
import System.IO

import qualified System.Directory as D

createDir :: FilePath -> IO ()
createDir dir = do
    p <- doesDirectoryExist dir
    if p
     then putStrLn $ dir <> " already exists, continuing ..."
     else do
         putStrLn ("Creating " <> dir <> " ...")
         createDirectoryIfMissing True dir

copyFiles :: Bool -> [FilePath] -> FilePath -> IO ()
copyFiles force fs dir = forM_ fs $ \from -> do
    let to = joinPath [dir, takeFileName from]
    p <- confirmCopy force to
    if p
     then putStrLn ("Writing " <> to <> " ...") >> D.copyFile from to
     else putStrLn $ "Skipping " <> to <> " ..."

confirmCopy :: Bool -> FilePath -> IO Bool
confirmCopy force to = do
    p <- doesFileExist to
    if not force && p
     then yesOrNo $ to <> " already exists, overwrite?"
     else return True

yesOrNo :: String -> IO Bool
yesOrNo prompt = do
    putStr $ prompt <> " [y/n]: "
    hFlush stdout
    str <- getLine
    case str of
        "y" -> return True
        "n" -> return False
        _   -> do
            putStrLn "Invalid response, please enter y or n ..."
            yesOrNo prompt
