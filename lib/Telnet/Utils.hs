-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet.Utils (
    doNvt,
    edge,
) where

import Telnet
import Telnet.Internal.Utils


edge :: NvtOpt -> NvtOpt -> NvtOpt
edge (NvtOptBool   p) opt@(NvtOptBool   q) | p /= q = opt
edge (NvtOptPair   p) opt@(NvtOptPair   q) | p /= q = opt
edge (NvtOptString p) opt@(NvtOptString q) | p /= q = opt
edge (NvtOptAlways p) opt@(NvtOptAlways q) | p /= q = opt
edge _                _                             = NvtOptNothing


doNvt :: NvtContext (IO ()) -> IO ()
doNvt = foldl1N (>>)
