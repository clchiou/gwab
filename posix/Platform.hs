-- Copyright (C) 2013 Che-Liang Chiou.

module Platform where

import qualified Data.String.Utils as Utils


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace = Utils.replace
