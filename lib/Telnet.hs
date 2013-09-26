-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet (
    Packet(..),
    parse,
    serialize,

    Nvt(..),
    applyOption,
    doEdgeTrigger,
    step
) where

import Control.Monad

import StringFilter
import Utils


type CommandCode = Char
type OptionCode  = Char


rfc854_SE       = '\240' :: CommandCode -- End of subnegotiation
rfc854_NOP      = '\241' :: CommandCode -- NOP
rfc854_DATAMARK = '\242' :: CommandCode -- Data Mark
rfc854_BREAK    = '\243' :: CommandCode -- Break
rfc854_IP       = '\244' :: CommandCode -- Interrupt Process
rfc854_AO       = '\245' :: CommandCode -- Abort output
rfc854_AYT      = '\246' :: CommandCode -- Are you there
rfc854_EC       = '\247' :: CommandCode -- Erase character
rfc854_EL       = '\248' :: CommandCode -- Erase line
rfc854_GOAHEAD  = '\249' :: CommandCode -- Go ahead
rfc854_SB       = '\250' :: CommandCode -- Begin of subnegotiation
rfc854_WILL     = '\251' :: CommandCode -- WILL
rfc854_WONT     = '\252' :: CommandCode -- WON'T
rfc854_DO       = '\253' :: CommandCode -- DO
rfc854_DONT     = '\254' :: CommandCode -- DON'T
rfc854_IAC      = '\255' :: CommandCode -- IAC


rfc856_BINARY_TRANSMISSION = '\0' :: OptionCode
rfc857_ECHO                = '\1' :: OptionCode
rfc858_SUPPRESS_GOAHEAD    = '\3' :: OptionCode


--
-- Telnet : Wire Protocol
--


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
            | PacketWill      { optionCode :: OptionCode }
            | PacketWont      { optionCode :: OptionCode }
            | PacketDo        { optionCode :: OptionCode }
            | PacketDont      { optionCode :: OptionCode }
            | PacketText      { text :: String }
              deriving (Show)


-- NOTE: Errors of input are ignored; we shift one left and keep parsing.
parse :: String -> [Packet]
parse input@(_:_) =
    case runFilter filterTelnet input of
        Just (packet, rest) -> packet : parse rest
        Nothing             -> parse $ tail input
parse _ = []


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


filterTelnet :: StringFilter Packet
filterTelnet =
    (matchByte rfc854_IAC >>
        ( matchByteTable singletons
         `mplus`
         (matchByteTable negotiations >>= \makepkt ->
          getByte >>= \opt ->
          return $ makepkt opt)
         `mplus`
         (matchByte rfc854_SB >>
          spanIac >>= \subopt ->
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
    (spanIac >>= \text ->
     if null text
     then fail "Empty text"
     else return $ PacketText text)
    where spanIac = withInput $ span_ (/= rfc854_IAC)


singletons :: [(CommandCode, Packet)]
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


negotiations :: [(CommandCode, OptionCode -> Packet)]
negotiations = [
    (rfc854_WILL, PacketWill),
    (rfc854_WONT, PacketWont),
    (rfc854_DO,   PacketDo),
    (rfc854_DONT, PacketDont)]


--
-- Telnet : Network Virtual Terminal
--


type Option = Maybe (Bool, Bool -> IO ())


-- Network Virtual Terminal
data Nvt = Nvt {
    -- RFC-856 Binary Transmission Mode
    binaryMode :: Option,

    -- RFC-857 ECHO
    echo       :: Option,

    -- RFC-858 Suppress GOAHEAD
    supGoAhead :: Option
}


instance Eq Nvt where
    nvt0 == nvt1 =
        binaryMode nvt0 === binaryMode nvt1 &&
        echo       nvt0 === echo       nvt1 &&
        supGoAhead nvt0 === supGoAhead nvt1
        where (===) (Just (flag0, _)) (Just (flag1, _)) = flag0 == flag1
              (===) Nothing           Nothing           = True
              (===) _                 _                 = False


instance Show Nvt where
    show nvt = "Nvt {" ++
        "binaryMode = " ++ (show' $ binaryMode nvt) ++ ", " ++
        "echo = "       ++ (show' $ echo       nvt) ++ ", " ++
        "supGoAhead = " ++ (show' $ supGoAhead nvt) ++ "}"
        where show' (Just (flag, _)) = show flag
              show' Nothing          = "?"


step :: Nvt -> Packet -> (Nvt, Maybe Packet)
step nvt0 packet@(PacketWill _) = stepNegotiation nvt0 packet True
step nvt0 packet@(PacketDo   _) = stepNegotiation nvt0 packet True
step nvt0 packet@(PacketWont _) = stepNegotiation nvt0 packet False
step nvt0 packet@(PacketDont _) = stepNegotiation nvt0 packet False
step nvt0 _                     = (nvt0, Nothing)


stepNegotiation :: Nvt -> Packet -> Bool -> (Nvt, Maybe Packet)
stepNegotiation nvt0 packet flag =
    case getOption opt >>= ($ nvt0) of
        Nothing            -> (nvt0, Just $ nak packet)
        Just (_, doOption) -> (setOption nvt0 opt option', Just $ ack packet)
            where option' = Just (flag, doOption)
    where opt = optionCode packet

          getOption = flip lookup [
                (rfc856_BINARY_TRANSMISSION, binaryMode),
                (rfc857_ECHO,                echo),
                (rfc858_SUPPRESS_GOAHEAD,    supGoAhead)]

          setOption nvt opt option
              | opt == rfc856_BINARY_TRANSMISSION = nvt{binaryMode=option}
              | opt == rfc857_ECHO                = nvt{echo      =option}
              | opt == rfc858_SUPPRESS_GOAHEAD    = nvt{supGoAhead=option}
              | otherwise                         = undefined

          ack (PacketWill opt) = PacketDo   opt
          ack (PacketDo   opt) = PacketWill opt
          ack (PacketWont opt) = PacketDont opt
          ack (PacketDont opt) = PacketWont opt

          nak (PacketWill opt) = PacketDont opt
          nak (PacketDo   opt) = PacketWont opt
          nak (PacketWont opt) = PacketDont opt
          nak (PacketDont opt) = PacketWont opt


applyOption :: Option -> IO ()
applyOption (Just (flag, doOption)) = return flag >>= doOption
applyOption Nothing                 = return ()


doEdgeTrigger :: (Nvt -> Option) -> Nvt -> Nvt -> IO ()
doEdgeTrigger optionGetter nvt0 nvt1 =
    let opt0 = optionGetter nvt0
        opt1 = optionGetter nvt1
    in case (opt0, opt1) of
        (Just (flag0, _), Just (flag1, _)) | flag0 /= flag1 -> applyOption opt1
        _                                                   -> return ()
