-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet where


type Command = Char
type Option  = Char


rfc854_SE       = '\240' -- End of subnegotiation
rfc854_NOP      = '\241' -- NOP
rfc854_DATAMARK = '\242' -- Data Mark
rfc854_BREAK    = '\243' -- Break
rfc854_IP       = '\244' -- Interrupt Process
rfc854_AO       = '\245' -- Abort output
rfc854_AYT      = '\246' -- Are you there
rfc854_EC       = '\247' -- Erase character
rfc854_EL       = '\248' -- Erase line
rfc854_GOAHEAD  = '\249' -- Go ahead
rfc854_SB       = '\250' -- Begin of subnegotiation
rfc854_WILL     = '\251' -- WILL
rfc854_WONT     = '\252' -- WON'T
rfc854_DO       = '\253' -- DO
rfc854_DONT     = '\254' -- DON'T
rfc854_IAC      = '\255' -- IAC


rfc856_BINARY_TRANSMISSION = '\0' :: Option
rfc857_ECHO                = '\1' :: Option
rfc858_SUPPRESS_GOAHEAD    = '\3' :: Option


data Packet = PacketNop
            | PacketDataMark
            | PacketBreak
            | PacketIp
            | PacketAo
            | PacketAyt
            | PacketEc
            | PacketEl
            | PacketGoAhead
            | PacketSubOption { subOption :: String }
            | PacketWill      { option :: Option }
            | PacketWont      { option :: Option }
            | PacketDo        { option :: Option }
            | PacketDont      { option :: Option }
            | PacketText      { text :: String }
              deriving (Show)


-- Network Virtual Terminal
data Nvt = Nvt {
    binaryMode :: Bool, -- RFC-856 Binary Transmission Mode
    echo       :: Bool, -- RFC-857 ECHO
    supGoAhead :: Bool  -- RFC-858 Suppress GOAHEAD
} deriving (Eq, Show)


-- The default value of binaryMode and supGoAhead are chosen for pratical
-- purpose, and are not the default value defined in RFC.
defaultNvt = Nvt {
    binaryMode = True,
    echo       = False,
    supGoAhead = True
}


-- NOTE: Errors of input are ignored; we shift one left and keep parsing.
parse :: String -> [Packet]
parse input@(_:_) =
    case runFilter filterTelnet input of
        Just (packet, rest) -> packet : parse rest
        Nothing             -> parse $ tail input
parse _ = []


filterTelnet :: PacketFilter Packet
filterTelnet =
    (matchByte rfc854_IAC >>
        ( matchByteTable singletons
         `mplus`
         (matchByteTable negotiations >>= \makepkt ->
          getByte >>= \opt ->
          return $ makepkt opt)
         `mplus`
         (matchByte rfc854_SB >>
          spanByte (/= rfc854_IAC) >>= \subopt ->
          matchByte rfc854_IAC >>
          matchByte rfc854_SE >>
          (return $ PacketSubOption subopt))
         `mplus`
         (matchByte rfc854_IAC >>
          (return $ PacketText [rfc854_IAC]))
         `mplus`
         (getByte >>= \c ->
          fail $ "Could not parse command: " ++ [c])))
    `mplus`
    (spanByte (/= rfc854_IAC) >>= \text ->
     if null text
     then fail "Empty text"
     else return $ PacketText text)


singletons :: [(Command, Packet)]
singletons = [
    (rfc854_NOP,      PacketNop),
    (rfc854_DATAMARK, PacketDataMark),
    (rfc854_BREAK,    PacketBreak),
    (rfc854_IP,       PacketIp),
    (rfc854_AO,       PacketAo),
    (rfc854_AYT,      PacketAyt),
    (rfc854_EC,       PacketEc),
    (rfc854_EL,       PacketEl),
    (rfc854_GOAHEAD,  PacketGoAhead)]


negotiations :: [(Command, Option -> Packet)]
negotiations = [
    (rfc854_WILL, PacketWill),
    (rfc854_WONT, PacketWont),
    (rfc854_DO,   PacketDo),
    (rfc854_DONT, PacketDont)]


data PacketFilterState = PacketFilterState {
    input :: String
} deriving (Show)


newtype PacketFilter resultType = PacketFilter {
    run :: PacketFilterState -> Maybe (PacketFilterState, resultType)
}


runFilter :: PacketFilter resultType -> String -> Maybe (resultType, String)
runFilter packetFilter inputString =
    case run packetFilter (PacketFilterState inputString) of
        Just (state, result) -> Just (result, input state)
        Nothing              -> Nothing


instance Monad PacketFilter where
    return result  = PacketFilter (\state -> Just (state, result))
    fail   message = PacketFilter (\_ -> Nothing)
    filter0 >>= makeFilter1 = PacketFilter chainedFilter
        where chainedFilter state0 =
                case run filter0 state0 of
                    Just (state1, result) -> run (makeFilter1 result) state1
                    Nothing               -> Nothing


mzero :: PacketFilter resultType
mzero = PacketFilter (\_ -> Nothing)


mplus :: PacketFilter resultType -> PacketFilter resultType ->
         PacketFilter resultType
mplus filter0 filter1 = PacketFilter chainedFilter
    where chainedFilter state0 =
            case run filter0 state0 of
                success@(Just _) -> success
                Nothing          -> run filter1 state0


getState :: PacketFilter PacketFilterState
getState = PacketFilter (\state -> Just (state, state))


putState :: PacketFilterState -> PacketFilter ()
putState state = PacketFilter (\_ -> Just (state, ()))


getByte :: PacketFilter Char
getByte =
    getState >>= \state ->
    case input state of
        (c:cs) -> putState state { input = cs } >>= \_ -> return c
        _      -> fail "No more input"


spanByte ::  (Char -> Bool) -> PacketFilter String
spanByte pred =
    getState >>= \state ->
    let (prefix, suffix) = span pred (input state)
    in putState state { input = suffix } >>= \_ -> return prefix


matchByte :: Char -> PacketFilter ()
matchByte expChar =
    getByte >>= \c ->
    if expChar == c
    then return ()
    else fail ("Expect " ++ [expChar] ++ " but got " ++ [c])


matchByteTable :: [(Char, result)] -> PacketFilter result
matchByteTable table =
    getByte >>= \c ->
    case lookup c table of
        Just result -> return result
        Nothing     -> fail ("Could not match " ++ [c])


putPacket :: Packet -> PacketFilter Packet
putPacket packet = PacketFilter (\state -> Just (state, packet))


negotiate :: Nvt -> Packet -> (Nvt, Maybe Packet)
negotiate nvt (PacketWill opt) = (nvt, Just $ PacketDont opt)
negotiate nvt (PacketDo   opt) = (nvt, Just $ PacketWont opt)
negotiate nvt (PacketWont opt) = (nvt, Just $ PacketDont opt)
negotiate nvt (PacketDont opt) = (nvt, Just $ PacketWont opt)
negotiate nvt _                = (nvt, Nothing)
{-
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
-}


serialize :: Packet -> String
serialize PacketNop      = [rfc854_IAC, rfc854_NOP]
serialize PacketDataMark = [rfc854_IAC, rfc854_DATAMARK]
serialize PacketBreak    = [rfc854_IAC, rfc854_BREAK]
serialize PacketIp       = [rfc854_IAC, rfc854_IP]
serialize PacketAo       = [rfc854_IAC, rfc854_AO]
serialize PacketAyt      = [rfc854_IAC, rfc854_AYT]
serialize PacketEc       = [rfc854_IAC, rfc854_EC]
serialize PacketEl       = [rfc854_IAC, rfc854_EL]
serialize PacketGoAhead  = [rfc854_IAC, rfc854_GOAHEAD]
serialize (PacketText text)        = text
serialize (PacketWill opt)         = [rfc854_IAC, rfc854_WILL, opt]
serialize (PacketWont opt)         = [rfc854_IAC, rfc854_WONT, opt]
serialize (PacketDo   opt)         = [rfc854_IAC, rfc854_DO,   opt]
serialize (PacketDont opt)         = [rfc854_IAC, rfc854_DONT, opt]
serialize (PacketSubOption subopt) =
    [rfc854_IAC, rfc854_SB] ++ subopt ++ [rfc854_IAC, rfc854_SE]
