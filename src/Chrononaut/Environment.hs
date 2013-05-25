{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chrononaut.Environment where

import Chrononaut.Types
import Control.Arrow
import Control.Monad.State
import System.Directory
import System.Exit
import System.IO

import qualified System.Process as P

localEnv :: FilePath
localEnv = "./.env"

loadEnvs :: [FilePath] -> IO Env
loadEnvs paths = do
    p <- doesFileExist localEnv
    s <- mapM readFile $ if p then paths ++ [localEnv] else paths
    expandVars s

expandVars :: [String] -> IO Env
expandVars vars = do
    (Nothing, Just h, Nothing, p) <- P.createProcess $ expandEnv vars
    !c <- P.waitForProcess p
    !s <- hGetContents h
    hClose h
    when (c /= ExitSuccess)
         (error "Failed to get environment variables")
    return $! parseEnv s

expandEnv :: [String] -> P.CreateProcess
expandEnv vars = (P.shell $ replaceBreaks (concat vars) ++ "env")
    { P.std_out = P.CreatePipe
    }

parseEnv :: String -> Env
parseEnv = map (second tail . break (== '=')) . lines

replaceBreaks :: String -> String
replaceBreaks = map f
  where
    f '\n' = ' '
    f c    = c
