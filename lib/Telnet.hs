-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet where

import qualified Data.Char as Char (chr, ord)


rfc854_SE       = Char.chr 240 -- End of subnegotiation
rfc854_NOP      = Char.chr 241 -- NOP
rfc854_DATAMARK = Char.chr 242 -- Data Mark
rfc854_BREAK    = Char.chr 243 -- Break
rfc854_IP       = Char.chr 244 -- Interrupt Process
rfc854_AO       = Char.chr 245 -- Abort output
rfc854_AYT      = Char.chr 246 -- Are you there
rfc854_EC       = Char.chr 247 -- Erase character
rfc854_EL       = Char.chr 248 -- Erase line
rfc854_GOAHEAD  = Char.chr 249 -- Go ahead
rfc854_SB       = Char.chr 250 -- Begin of subnegotiation
rfc854_WILL     = Char.chr 251 -- WILL
rfc854_WONT     = Char.chr 252 -- WON'T
rfc854_DO       = Char.chr 253 -- DO
rfc854_DONT     = Char.chr 254 -- DON'T
rfc854_IAC      = Char.chr 255 -- IAC


data Packet = Text  { getText :: String }
            | Se
            | Nop
            | DataMark
            | Break
            | Ip
            | Ao
            | Ayt
            | Ec
            | El
            | GoAhead
            | Sb
            | Will  { getOption :: Int }
            | Wont  { getOption :: Int }
            | Do    { getOption :: Int }
            | Dont  { getOption :: Int }
            | Iac   -- IAC sent as data
              deriving (Show)


serialize :: Packet -> String
serialize (Text text) = text
serialize Se          = [rfc854_IAC, rfc854_SE]
serialize Nop         = [rfc854_IAC, rfc854_NOP]
serialize DataMark    = [rfc854_IAC, rfc854_DATAMARK]
serialize Break       = [rfc854_IAC, rfc854_BREAK]
serialize Ip          = [rfc854_IAC, rfc854_IP]
serialize Ao          = [rfc854_IAC, rfc854_AO]
serialize Ayt         = [rfc854_IAC, rfc854_AYT]
serialize Ec          = [rfc854_IAC, rfc854_EC]
serialize El          = [rfc854_IAC, rfc854_EL]
serialize GoAhead     = [rfc854_IAC, rfc854_GOAHEAD]
serialize Sb          = [rfc854_IAC, rfc854_SB]
serialize (Will opt)  = [rfc854_IAC, rfc854_WILL, Char.chr opt]
serialize (Wont opt)  = [rfc854_IAC, rfc854_WONT, Char.chr opt]
serialize (Do   opt)  = [rfc854_IAC, rfc854_DO,   Char.chr opt]
serialize (Dont opt)  = [rfc854_IAC, rfc854_DONT, Char.chr opt]
serialize Iac         = [rfc854_IAC, rfc854_IAC]


parse :: String -> [Packet]
parse cs@(_:_) =
    let (text, command) = span (/= rfc854_IAC) cs
        ps              = parseCommand command
    in if null text
    then ps
    else (Text text):ps
parse _ = []


parseCommand :: String -> [Packet]
parseCommand (iac:c:cs) =
    if iac /= rfc854_IAC         then error ("Could not find IAC: " ++ [iac])
    else if c == rfc854_SE       then Se       : ps
    else if c == rfc854_NOP      then Nop      : ps
    else if c == rfc854_DATAMARK then DataMark : ps
    else if c == rfc854_BREAK    then Break    : ps
    else if c == rfc854_IP       then Ip       : ps
    else if c == rfc854_AO       then Ao       : ps
    else if c == rfc854_AYT      then Ayt      : ps
    else if c == rfc854_EC       then Ec       : ps
    else if c == rfc854_EL       then El       : ps
    else if c == rfc854_GOAHEAD  then GoAhead  : ps
    else if c == rfc854_SB       then Sb       : ps
    else if c == rfc854_IAC      then Iac      : ps
    else case cs of
        (opt:cs') | c == rfc854_WILL -> (Will $ Char.ord opt) : ps'
                  | c == rfc854_WONT -> (Wont $ Char.ord opt) : ps'
                  | c == rfc854_DO   -> (Do   $ Char.ord opt) : ps'
                  | c == rfc854_DONT -> (Dont $ Char.ord opt) : ps'
                  | otherwise        -> error ("Could not recognize: " ++ [c])
            where ps' = parse cs'
        _ -> error "Could not recognize command format"
    where ps = parse cs
parseCommand _ = []
