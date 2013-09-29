-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet (
    Packet(..),
    parse,
    serialize,

    NvtContext(..),
    doNvt,
    edgeTrigger,
    step,
) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Platform (replace)
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
              deriving (Eq, Show)


-- NOTE: Errors of input are ignored; we shift one left and keep parsing.
parse :: String -> [Packet]
parse input@(_:_) =
    case runFilter filterTelnet input of
        Just (packet, rest) -> packet : parse rest
        Nothing             -> parse $ tail input
parse _ = []


serialize :: Packet -> String
serialize PacketNop                = [rfc854_IAC, rfc854_NOP]
serialize PacketDataMark           = [rfc854_IAC, rfc854_DATAMARK]
serialize PacketBreak              = [rfc854_IAC, rfc854_BREAK]
serialize PacketIp                 = [rfc854_IAC, rfc854_IP]
serialize PacketAo                 = [rfc854_IAC, rfc854_AO]
serialize PacketAyt                = [rfc854_IAC, rfc854_AYT]
serialize PacketEc                 = [rfc854_IAC, rfc854_EC]
serialize PacketEl                 = [rfc854_IAC, rfc854_EL]
serialize PacketGoAhead            = [rfc854_IAC, rfc854_GOAHEAD]
serialize (PacketWill opt)         = [rfc854_IAC, rfc854_WILL, opt]
serialize (PacketWont opt)         = [rfc854_IAC, rfc854_WONT, opt]
serialize (PacketDo   opt)         = [rfc854_IAC, rfc854_DO,   opt]
serialize (PacketDont opt)         = [rfc854_IAC, rfc854_DONT, opt]
serialize (PacketSubOption subopt) = [rfc854_IAC, rfc854_SB] ++
                                     quote subopt            ++
                                     [rfc854_IAC, rfc854_SE]
serialize (PacketText text)        = quote text


quote :: String -> String
quote = replace [rfc854_IAC] [rfc854_IAC, rfc854_IAC]


filterTelnet :: StringFilter Packet
filterTelnet =
    (matchByte rfc854_IAC >>
        ( matchByteTable singletons
         `mplus`
         (matchByteTable negotiations >>= \makepkt ->
          getByte >>=
          return . makepkt)
         `mplus`
         (matchByte rfc854_SB >>
          readUntilIac' >>= \subopt ->
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
    (readText >>=
     return . PacketText)
    where singletons = [
              (rfc854_NOP,      PacketNop),
              (rfc854_DATAMARK, PacketDataMark),
              (rfc854_BREAK,    PacketBreak),
              (rfc854_IP,       PacketIp),
              (rfc854_AO,       PacketAo),
              (rfc854_AYT,      PacketAyt),
              (rfc854_EC,       PacketEc),
              (rfc854_EL,       PacketEl),
              (rfc854_GOAHEAD,  PacketGoAhead)]
          negotiations = [
              (rfc854_WILL, PacketWill),
              (rfc854_WONT, PacketWont),
              (rfc854_DO,   PacketDo),
              (rfc854_DONT, PacketDont)]
          readUntilIac' = withInput $ readUntilIac []
          -- Text packets may be split in the middle; so we do not insist on
          -- reading until we see the start of next command.
          readText = getPrefix (/= rfc854_IAC)


-- NOTE: readUntilIac is NOT lazy; it continues to read until it sees an IAC
-- that is not sent as data, i.e., start of next command.  The non-laziness is
-- probably only useful when parsing subnegotiation where the suboption is not
-- complete until we see an SE command.
--
-- This function also unquotes IACs that are sent as data.
readUntilIac :: String -> String -> Maybe (String, String)
readUntilIac prefix suffix
    | null suffix =
        if null prefix
        then Nothing
        else Just (prefix, suffix)
    | otherwise =
        if iacSentAsData == [rfc854_IAC, rfc854_IAC]
        then readUntilIac (prefix ++ prefix' ++ [rfc854_IAC]) suffix''
        else Just (prefix ++ prefix', suffix')
        where (prefix', suffix')        = span (/= rfc854_IAC) suffix
              (iacSentAsData, suffix'') = splitAt 2 suffix'


--
-- Telnet : Network Virtual Terminal
--


data NvtContext a = NvtContext {
    -- RFC-856 Binary Transmission
    binary     :: a,

    -- RFC-857 ECHO
    echo       :: a,

    -- RFC-858 Suppress GOAHEAD
    supGoAhead :: a
}


nvtOptionCode :: NvtContext OptionCode
nvtOptionCode  = NvtContext {
    binary     = rfc856_BINARY_TRANSMISSION,
    echo       = rfc857_ECHO,
    supGoAhead = rfc858_SUPPRESS_GOAHEAD
}


instance Functor NvtContext where
    fmap f a = NvtContext {
        binary     = f $ binary     a,
        echo       = f $ echo       a,
        supGoAhead = f $ supGoAhead a
    }


instance Applicative NvtContext where
    pure v = NvtContext {
        binary     = v,
        echo       = v,
        supGoAhead = v
    }

    f <*> a = NvtContext {
        binary     = binary     f (binary     a),
        echo       = echo       f (echo       a),
        supGoAhead = supGoAhead f (supGoAhead a)
    }


instance Eq a => Eq (NvtContext a) where
    nvt0 == nvt1 =
        binary     nvt0 == binary     nvt1 &&
        echo       nvt0 == echo       nvt1 &&
        supGoAhead nvt0 == supGoAhead nvt1


instance Show a => Show (NvtContext a) where
    show nvtcxt = "NvtContext {" ++
        "binary = "     ++ binary     nvtcxt' ++ ", " ++
        "echo = "       ++ echo       nvtcxt' ++ ", " ++
        "supGoAhead = " ++ supGoAhead nvtcxt' ++ "}"
        where nvtcxt' = show `fmap` nvtcxt


foldlN :: (a -> b -> a) -> a -> NvtContext b -> a
foldlN f z nvt =
    z `f` binary nvt `f` echo nvt `f` supGoAhead nvt


type Nvt    = NvtContext Option
type Option = Maybe Bool


step :: Nvt -> Packet -> (Nvt, Maybe Packet)
step nvt packet@(PacketWill _) = stepNegotiation nvt packet True
step nvt packet@(PacketDo   _) = stepNegotiation nvt packet True
step nvt packet@(PacketWont _) = stepNegotiation nvt packet False
step nvt packet@(PacketDont _) = stepNegotiation nvt packet False
step nvt _                     = (nvt, Nothing)


stepNegotiation :: Nvt -> Packet -> Bool -> (Nvt, Maybe Packet)
stepNegotiation nvt packet flag =
    (setOption opt flag nvt, Just (makeResponse opt nvt packet))
    where opt = optionCode packet


selectOption :: OptionCode -> NvtContext Bool
selectOption = liftA2 (==) nvtOptionCode . pure


setOption :: OptionCode -> Bool -> Nvt -> Nvt
setOption opt flag nvt0 =
    (select &&& flag') ||| (not' select &&& nvt0)
    where select = fmap Just (selectOption opt)
          flag'  = pure (Just flag)
          not'   = liftA (liftA not)
          (&&&)  = liftA2 (liftA2 (&&))
          (|||)  = liftA2 (liftA2 (||))


makeResponse :: OptionCode -> Nvt -> Packet -> Packet
makeResponse opt nvt packet = pktgen packet
    where select  = selectOption opt
          pktgens = fmap (\option -> if isJust option then ack else nak) nvt
          pktgen  = foldlN
                    (\zero (select, pktgen) -> if select then pktgen else zero)
                    nak
                    (liftA2 (,) select pktgens)


ack :: Packet -> Packet
ack (PacketWill opt) = PacketDo   opt
ack (PacketDo   opt) = PacketWill opt
ack (PacketWont opt) = PacketDont opt
ack (PacketDont opt) = PacketWont opt


nak :: Packet -> Packet
nak (PacketWill opt) = PacketDont opt
nak (PacketDo   opt) = PacketWont opt
nak (PacketWont opt) = PacketDont opt
nak (PacketDont opt) = PacketWont opt


edgeTrigger :: Option -> Option -> Option
edgeTrigger (Just a) (Just b) | a /= b = Just b
edgeTrigger _        _                 = Nothing


doNvt :: NvtContext (IO ()) -> IO ()
doNvt = foldlN (>>) (return ())
