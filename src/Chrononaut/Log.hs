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

import Chrononaut.Schema
import Chrononaut.Types
import Control.Applicative
import Control.Monad.CatchIO
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

getLog :: (Applicative m, MonadCatchIO m) => FilePath -> App m Log
getLog dir = do
    rs  <- liftIO $ getDirectoryContents dir
    cur <- getRevision
    return $! Log cur . nub . sort $ mapMaybe fromPath rs

append :: MonadIO m => String -> Log -> App m Log
append desc log = do
    !cur <- liftIO getCurrentTime

    let ts   = timeStamp cur
        rev  = Rev ts (underscore desc)
        up   = fileName "up"   rev
        down = fileName "down" rev
        ctx  = Ctx desc (show ts) (show cur) up down

    render up   "migrate"  ctx
    render down "rollback" ctx

    return $! Log (logCur log) (logRevs log ++ [rev])

render :: (MonadIO m, Data a) => String -> String -> a -> App m ()
render path tmpl ctx = do
    t <- templatePath tmpl
    f <- rootPath path
    liftIO $ do
        !bs <- hastacheFile defaultConfig t $ mkGenericContext ctx
        putStrLn $ "Writing " <> f <> " ..."
        BL.writeFile f bs

current :: Log -> Maybe Rev
current log = logCur log >>= (`byId` logRevs log)

diff :: Int -> Maybe Int -> Log -> Either String [Rev]
diff step n log =
    case (partition step n . fst $ pending log, n) of
        (Just (ps, _), Nothing) ->
            Right ps
        (Nothing, Just x) ->
            Left $ "Unable to find revision matching " ++ show x
        (_, _) ->
            Left $ "Step " ++ show step ++ " is out of range"

partition :: Int -> Maybe Int -> [Rev] -> Maybe ([Rev], [Rev])
partition step n rs = maybe (partitionStep step rs) (`partitionId` rs) n

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

fileName :: String -> Rev -> FilePath
fileName mode (Rev n s) = take 250 parts ++ ".sql"
  where
    parts = show n ++ "-" ++ mode ++ "-" ++ s

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
