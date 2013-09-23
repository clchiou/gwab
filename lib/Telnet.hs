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


type Option = Int


rfc856_BINARY_TRANSMISSION = 0 :: Option
rfc857_ECHO                = 1 :: Option
rfc858_SUPPRESS_GOAHEAD    = 3 :: Option


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
            | Will  { getOption :: Option }
            | Wont  { getOption :: Option }
            | Do    { getOption :: Option }
            | Dont  { getOption :: Option }
            | Iac   { getIac :: Char } -- IAC sent as data
            -- Sub-option between SB and SE
            | SubOption { getSubOption :: String }
              deriving (Show)


-- Network Virtual Terminal
data Nvt = Nvt {
    getBinary     :: Bool,
    getEcho       :: Bool,
    getSupGoAhead :: Bool
} deriving (Eq, Show)


defaultNvt = Nvt {
    getBinary     = True,  -- Binary transmission mode; This is unorthodox...
    getEcho       = False, -- Won't ECHO
    getSupGoAhead = True   -- Suppress GOAHEAD; This is unorthodox...
}


negotiate :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiate nvt p@(Will _) = negotiate' nvt p
negotiate nvt p@(Do   _) = negotiate' nvt p
negotiate nvt p@(Wont _) = negotiate' nvt p
negotiate nvt p@(Dont _) = negotiate' nvt p
negotiate nvt _          = (nvt, Nothing)


negotiators :: [(Option, Nvt -> Packet -> (Nvt, Maybe Packet))]
negotiators = [
    (rfc856_BINARY_TRANSMISSION, negotiateBinary),
    (rfc857_ECHO,                negotiateEcho),
    (rfc858_SUPPRESS_GOAHEAD,    negotiateSupGoAhead)]


negotiate' :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiate' nvt p =
    case lookup (getOption p) negotiators of
        Just negotiator -> negotiator nvt p
        Nothing         -> negotiatorNone
    where negotiatorNone = -- Respond to unsupported options
            case p of
                (Will opt) -> (nvt, Just (Dont opt))
                (Do   opt) -> (nvt, Just (Wont opt))
                (Wont opt) -> (nvt, Just (Dont opt))
                (Dont opt) -> (nvt, Just (Wont opt))


negotiateBinary :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiateBinary nvt (Will _) = (nvt {getBinary = True},
                                Just (Do   rfc856_BINARY_TRANSMISSION))
negotiateBinary nvt (Do   _) = (nvt {getBinary = True},
                                Just (Will rfc856_BINARY_TRANSMISSION))
negotiateBinary nvt (Wont _) = (nvt {getBinary = False},
                                Just (Dont rfc856_BINARY_TRANSMISSION))
negotiateBinary nvt (Dont _) = (nvt {getBinary = False},
                                Just (Wont rfc856_BINARY_TRANSMISSION))


negotiateEcho :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiateEcho nvt (Will _) = (nvt {getEcho = True},  Just (Do   rfc857_ECHO))
negotiateEcho nvt (Do   _) = (nvt {getEcho = True},  Just (Will rfc857_ECHO))
negotiateEcho nvt (Wont _) = (nvt {getEcho = False}, Just (Dont rfc857_ECHO))
negotiateEcho nvt (Dont _) = (nvt {getEcho = False}, Just (Wont rfc857_ECHO))


-- XXX: This is wrong! We do not honor GoAhead at all.
negotiateSupGoAhead :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiateSupGoAhead nvt (Will _) = (nvt {getSupGoAhead = True},
                                    Just (Do   rfc858_SUPPRESS_GOAHEAD))
negotiateSupGoAhead nvt (Do   _) = (nvt {getSupGoAhead = True},
                                    Just (Will rfc858_SUPPRESS_GOAHEAD))
negotiateSupGoAhead nvt (Wont _) = (nvt {getSupGoAhead = False},
                                    Just (Dont rfc858_SUPPRESS_GOAHEAD))
negotiateSupGoAhead nvt (Dont _) = (nvt {getSupGoAhead = False},
                                    Just (Wont rfc858_SUPPRESS_GOAHEAD))


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
serialize (Iac  _)    = [rfc854_IAC, rfc854_IAC]
serialize (SubOption opt) = opt


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
    else if c == rfc854_SB       then Sb       : parseSubOption cs
    else if c == rfc854_IAC      then (Iac rfc854_IAC) : ps
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


-- XXX: This function does not handle IAC-as-data in sub-option and is thus
--      incorrect.  You should rewrite the whole parser later.
parseSubOption :: String -> [Packet]
parseSubOption cs@(_:_) =
    let (subopt, command) = span (/= rfc854_IAC) cs
    in (SubOption subopt) : parseCommand command
parseSubOption _ = []
