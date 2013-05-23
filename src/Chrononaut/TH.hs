module Chrononaut.TH (
    -- * Heredoc Strings
      line
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

line :: QuasiQuoter
line = QuasiQuoter
    { quoteExp  = stringE . unwords . words
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
