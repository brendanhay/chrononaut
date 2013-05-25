module Chrononaut.Commands (
    -- * CLI
      initialise
    , status
    , create
    , migrate
    , rollback
    , redo
    ) where

import Prelude hiding (log)

import Chrononaut.Application
import Control.Monad.IO.Class

import Chrononaut.Log

initialise :: Bool -> FilePath -> IO ()
initialise = setup

status :: FilePath -> [FilePath] -> String -> IO ()
status = runApp (withLog (liftIO . putStr . show))

create :: String -> FilePath -> IO ()
create desc root = ensureSetup root >> getLog Nothing root >>= append desc

migrate :: Maybe Int -> Maybe Int -> FilePath -> [FilePath] -> String -> IO ()
migrate rev step = runApp . withLog $
    \log -> either error (liftIO . mapM_ print)
                   (diff step rev log)

rollback :: Maybe Int -> Maybe Int -> FilePath -> [FilePath] -> String -> IO ()
rollback rev step = runApp . withLog $
    \log -> either error (liftIO . mapM_ print)
                   (diff step' rev log)
  where
    step' = fmap (-1) $ maybe 1 id step

redo :: Maybe Int -> Maybe Int -> FilePath -> [FilePath] -> String -> IO ()
redo rev step = runApp . withLog $
    \log -> either error (liftIO . mapM_ print)
                   (diff step rev log)
