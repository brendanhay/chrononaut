module Main (main) where

import Chrononaut.Command
import Control.Applicative
import Data.Version              (showVersion)
import System.Console.CmdTheLine
import System.Environment

import qualified Paths_chrononaut as P

main :: IO ()
main = do
    name <- getProgName
    runChoice (defTerm name)
        [ initTerm     "init"
        , statusTerm   "status"
        , createTerm   "create"
        , migrateTerm  "migrate"
        , rollbackTerm "rollback"
        , redoTerm     "redo"
        , testTerm     "test"
        ]

defTerm :: String -> (Term (IO ()), TermInfo)
defTerm name = (term, info)
  where
    term = ret $ (\_ _ -> helpFail Pager Nothing)
        <$> directory
        <*> force

    info = (describe
        " \
        \ \
        \.")
        { version  = showVersion P.version
        , termName = name
        }

initTerm :: String -> (Term (IO ()), TermInfo)
initTerm name = (term, info)
  where
    term = initialise
        <$> directory
        <*> force

    info = (describe
        "Init.")
        { termName = name
        , termDoc  = "Init."
        }

statusTerm :: String -> (Term (IO ()), TermInfo)
statusTerm name = (term, info)
  where
    term = status
        <$> directory
        <*> envs

    info = (describe
        "Status.")
        { termName = name
        , termDoc  = "Status."
        }

createTerm :: String -> (Term (IO ()), TermInfo)
createTerm name = (term, info)
  where
    term = create
        <$> directory
        <*> description

    info = (describe
        "Create.")
        { termName = name
        , termDoc  = "Create."
        }

migrateTerm :: String -> (Term (IO ()), TermInfo)
migrateTerm name = (term, info)
  where
    term = migrate
        <$> directory
        <*> force
        <*> step
        <*> revision
        <*> envs

    info = (describe
        "Migrate.")
        { termName = name
        , termDoc  = "Migrate."
        }

rollbackTerm :: String -> (Term (IO ()), TermInfo)
rollbackTerm name = (term, info)
  where
    term = rollback
        <$> directory
        <*> force
        <*> step
        <*> revision
        <*> envs

    info = (describe
        "Rollback.")
        { termName = name
        , termDoc  = "Rollback."
        }

redoTerm :: String -> (Term (IO ()), TermInfo)
redoTerm name = (term, info)
  where
    term = redo
        <$> directory
        <*> force
        <*> step
        <*> revision
        <*> envs

    info = (describe
        "Redo.")
        { termName = name
        , termDoc  = "Redo."
        }

testTerm :: String -> (Term (IO ()), TermInfo)
testTerm name = (term, info)
  where
    term = test
        <$> directory
        <*> envs

    info = (describe
        "Test.")
        { termName = name
        , termDoc  = "Test."
        }

common :: String
common = "COMMON OPTIONS"

describe :: String -> TermInfo
describe desc = defTI
    { man =
        [ S "DESCRIPTION"
        , P desc
        , S common
        , S "MORE HELP"
        , P "Use '$(mname) $(i,COMMAND) --help' for help on a single command."
        ]
    }

directory :: Term FilePath
directory = value . opt "db" $ (optInfo ["dir"])
    { optDoc = "Root directory containing templates and the \
               \migrate sub-directory."
    , optSec = common
    }

force :: Term Bool
force = value $ flag (optInfo ["force"])
    { optDoc = "Force operation and ignore any y/n prompts."
    , optSec = common
    }

envs :: Term [FilePath]
envs = value . optAll [] $ (optInfo ["env"])
    { optDoc = "Foreman style .env files to merge into then environment \
               \used to run commands. Can be repeatedly specified. \
               \If a .env file exists in the working directory, it will be \
               \loaded and merged with with the current environment."
    }

description :: Term String
description = required $ pos 0 Nothing posInfo { posName = "DESCRIPTION" }

step :: Term (Maybe Int)
step = value . opt Nothing $ (optInfo ["step"])
    { optDoc = "Number of migration versions to run. \
               \Implies --revision=<current+step>."
    }

revision :: Term (Maybe Int)
revision = value . opt Nothing $ (optInfo ["revision"])
    { optDoc = "Migration revision to run from/to."
    }
