{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Chrononaut.Log (
    -- * Revision
      Rev

    -- * Log
    , Log
    , getLog
    , append
    , current
    , diff
    ) where

import Prelude                 hiding (log)

import Chrononaut.Path
import Control.Monad.IO.Class
import Data.Char
import Data.Data
import Data.List               hiding (partition)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Safe
import System.Directory
import System.FilePath
import System.Locale
import Text.Hastache
import Text.Hastache.Context
import Text.Printf

import qualified Data.ByteString.Lazy.Char8 as BL

maxLength :: Int
maxLength = 16

data Ctx = Ctx
    { description  :: !String
    , revision     :: !String
    , date         :: !String
    , migrate      :: !String
    , rollback     :: !String
    } deriving (Data, Typeable)

data Rev = Rev !Int !String deriving (Eq, Ord)

instance Show Rev where
    show (Rev n s) = printf ("%0" ++ show maxLength ++ "d") n ++ " - " ++ s

data Log = Log
    { logCur  :: !(Maybe Int)
    , logRevs :: ![Rev]
    , logDir  :: FilePath
    , tmplDir :: FilePath
    }

instance Show Log where
    show log@Log{..} = unlines $ intersperse "#"
        [ "# Your database is " ++ show (offset log) ++ " revision(s) behind."
        , "# Revision:"
        , "#   " ++ maybe "<PENDING>" show (current log)
        , "# Pending:"
        , f p
        , "# Applied:"
        , f a
        ]
      where
        f []   = "#"
        f xs   = intercalate "\n" $ map (("#   " ++) . show) xs
        (p, a) = pending log

getLog :: Maybe Int -> FilePath -> IO Log
getLog n root = do
    let dir = joinPath [root, "migrate"]
    createDirectoryIfMissing True dir
    rs <- getDirectoryContents dir
    return $! Log n (nub . sort $ mapMaybe fromPath rs) dir root

append :: String -> Log -> IO ()
append desc log = do
    !cur <- liftIO getCurrentTime

    let ts   = timeStamp cur
        rev  = Rev ts (underscore desc)
        up   = migratePath  rev log
        down = rollbackPath rev log
        ctx  = Ctx desc (show ts) (show cur) (snd up) (snd down)

    render up   ctx
    render down ctx

render :: Data a => (FilePath, FilePath) -> a -> IO ()
render (tmpl, path) ctx = do
    bs <- hastacheFile defaultConfig tmpl $ mkGenericContext ctx
    putStrLn $ "Writing " <> path <> " ..."
    BL.writeFile path bs

current :: Log -> Maybe Rev
current log = logCur log >>= (`byId` logRevs log)

diff :: Maybe Int -> Maybe Int -> Log -> Either String [Rev]
diff n step log =
    case (partition step n . fst $ pending log, n) of
        (Just (ps, _), Nothing) ->
            Right ps
        (Nothing, Just x) ->
            Left $ "Unable to find revision matching " ++ show x
        (_, _) ->
            Left $ "Step " ++ show step ++ " is out of range"

partition :: Maybe Int -> Maybe Int -> [Rev] -> Maybe ([Rev], [Rev])
partition rev step rs = case (rev, step) of
    (Just  x, Nothing) -> x `partitionId` rs
    (Nothing, Just  x) -> x `partitionStep` rs
    _                  -> Just (rs, [])

partitionId :: Int -> [Rev] -> Maybe ([Rev], [Rev])
partitionId n rs = do
    idx <- findIndex (\(Rev j _) -> n == j) rs
    return $! splitAt idx rs

partitionStep :: Int -> [Rev] -> Maybe ([Rev], [Rev])
partitionStep step rs =
    if step > length rs
       then Nothing
       else Just $ splitAt step rs

pending :: Log -> ([Rev], [Rev])
pending log = splitAt (offset log) $ logRevs log

offset :: Log -> Int
offset log@Log{..} = fromMaybe
    (length logRevs)
    (current log >>= (`elemIndex` logRevs))

byId :: Int -> [Rev] -> Maybe Rev
byId n = find (\(Rev j _) -> n == j)

fromPath :: FilePath -> Maybe Rev
fromPath path = do
    rev <- headMay xs >>= readMay
    return $! Rev rev (tail . unwords $ drop 2 xs)
  where
    xs = split (== '-') . dropExtension $ takeFileName path

migratePath, rollbackPath :: Rev -> Log -> (FilePath, FilePath)
migratePath  rev log = (tmplPath "migrate" log,  filePath "up" rev log)
rollbackPath rev log = (tmplPath "rollback" log, filePath "down" rev log)

tmplPath :: String -> Log -> FilePath
tmplPath name = joinPaths (name ++ ".tmpl") . tmplDir

filePath :: String -> Rev -> Log -> FilePath
filePath name (Rev n s) = joinPaths (take 150 parts ++ ".sql") . logDir
  where
    parts = show n ++ "-" ++ name ++ "-" ++ s

timeStamp :: UTCTime -> Int
timeStamp =
    read . take maxLength . formatTime defaultTimeLocale "%Y%m%d%M%S%q"

underscore :: String -> String
underscore = map toLower . init . concatMap g . split f
  where
    f x = any ($ x) [isUpper, isSpace]
    g []       = []
    g (' ':xs) = g xs
    g ('_':xs) = g xs
    g xs | last xs == '_' = xs
         | otherwise      = xs ++ "_"

split :: (Char -> Bool) -> String -> [String]
split p s
    | [] <- b   = [match]
    | [] <- c   = [match ++ rest]
    | otherwise = (match ++ rest) : split p c
  where
    (match, b) = span  p s
    (rest,  c) = break p b
