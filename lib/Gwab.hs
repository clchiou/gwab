-- Copyright (C) 2013 Che-Liang Chiou.

module Gwab (
    GwabState,
) where

import Control.Applicative

import Telnet (
    Nvt,
    NvtContext,
    NvtOpt(..),
    Packet,
    step)

import Telnet.Utils (
    edge,
    sequenceNvt)

import Terminal (
    Frame,
    Sequence)


data GwabState = GwabState {
    frame    :: Frame,
    nvt      :: Nvt
} deriving (Eq, Show)


type Callback = NvtContext (NvtOpt -> IO ())


step :: Callback -> GwabState -> Packet -> (GwabState, IO (), [Packet])
step callback (GwabState frame nvt) packet = (state, io, ps) where
    state = GwabState frame' nvt'

    frame'     = draw packet frame
    (nvt', ps) = Telnet.step nvt packet

    io = sequenceNvt (apply <$> callback <*> (edge <$> nvt <*> nvt'))
    apply f NvtOptNothing = return ()
    apply f nvtOpt        = f nvtOpt


draw :: Packet -> Frame -> Frame
draw packet frame = undefined
