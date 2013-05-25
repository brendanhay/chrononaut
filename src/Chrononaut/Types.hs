{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chrononaut.Types (
    -- * Exported Types
      Env
    , App
    , HasDB(withDB)

    -- * Opaque Accessors
    , newApp
    , rootDir
    ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString                   (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple hiding (connect)
import Network.URI                       (isAbsoluteURI)

import qualified Data.ByteString.Char8 as BS

type Env = [(String, String)]

newtype Url = Url ByteString deriving (Show)

newtype DB = DB { getDBPool :: Pool Connection }

data Chrono = Chrono
    { chronoRoot :: !FilePath
    , chronoDB   :: !DB
    }

type App = ReaderT Chrono

class MonadCatchIO m => HasDB m where
    withDB :: (Connection -> IO a) -> m a

instance MonadCatchIO m => HasDB (App m) where
    withDB f = ask >>= liftIO . (`withResource` f) . getDBPool . chronoDB

newApp :: FilePath -> Env -> String -> IO Chrono
newApp root env url = Chrono root <$> connect (parseUrl url env)

rootDir :: Monad m => App m FilePath
rootDir = chronoRoot `liftM` ask

connect :: Url -> IO DB
connect (Url bs) = DB <$> createPool (connectPostgreSQL bs) close 1 1 1

parseUrl :: String -> Env -> Url
parseUrl s env = validateUrl $
    if isAbsoluteURI s
     then f s
     else maybe (error $ "failed to read environment variable '" ++ s ++ "'")
                f (s `lookup` env)
  where
    f = Url . BS.pack

validateUrl :: Url -> Url
validateUrl u@(Url s) =
    if "pgsql" `BS.isPrefixOf` s
       then error $ "connection '" ++ BS.unpack s ++
                "' must start with postgresql:// scheme"
       else u
