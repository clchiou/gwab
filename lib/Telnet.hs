-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet (
    StringFilter.Error(Err, NeedMoreInput),

    Telnet.Consts.Nvt,
    Telnet.Consts.NvtContext(..),
    Telnet.Consts.NvtOpt(..),
    Telnet.Consts.Packet(..),
    Telnet.Consts.fromList,

    Telnet.Consts.rfc856_BINARY_TRANSMISSION,
    Telnet.Consts.rfc857_ECHO,
    Telnet.Consts.rfc858_SUPPRESS_GOAHEAD,
    Telnet.Consts.rfc1073_WINDOW_SIZE,
    Telnet.Consts.rfc1091_TERMINAL_TYPE,

    parse,
    serialize,
    step,
) where

import Control.Applicative
import Control.Monad
import Data.Map (intersectionWith, toList)
import Data.Maybe

import Platform (join)

import StringFilter
import StringFilter.Utils

import Telnet.Consts
import Telnet.Internal.Utils


--
-- Telnet : Wire Protocol
--


parse :: String -> FilterResult Packet
parse = runFilter filterTelnet


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


filterTelnet :: StringFilter Packet
filterTelnet =
    (matchByte (== rfc854_IAC) >>
        ( matchByte' (flip lookup singletons)
         `mplus`
         (matchByte' (flip lookup negotiations) >>= \makepkt ->
          getByte >>=
          return . makepkt)
         `mplus`
         (matchByte (== rfc854_SB) >>
          readUntilIac' >>= \subopt ->
          matchByte (== rfc854_IAC) >>
          matchByte (== rfc854_SE) >>
          (return $ PacketSubOption subopt))
         `mplus`
         (matchByte (== rfc854_IAC) >>
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


--
-- Telnet : Network Virtual Terminal
--


instance Functor NvtContext where
    fmap f (NvtContext table) = NvtContext $ fmap f table


instance Applicative NvtContext where
    pure v = makeNvtContext v
    (NvtContext f) <*> (NvtContext a) = NvtContext $ intersectionWith ($) f a


instance Show a => Show (NvtContext a) where
    show (NvtContext table) = "NvtContext {" ++ opts ++ "}" where
        opts  = Platform.join ", " $ map show' $ toList table
        show' (n, v) = prettyName n ++ " = " ++ show v
        prettyName n = case lookup n knownOptCodes of
            (Just name) -> name
            Nothing     -> show n
        knownOptCodes =
            [(rfc856_BINARY_TRANSMISSION, "binary"),
             (rfc857_ECHO,                "echo"),
             (rfc858_SUPPRESS_GOAHEAD,    "supGoAhead"),
             (rfc1073_WINDOW_SIZE,        "windowSize"),
             (rfc1091_TERMINAL_TYPE,      "termType")]


step :: Nvt -> Packet -> (Nvt, [Packet])
step nvt packet@(PacketWill      _) = negotiate nvt packet
step nvt packet@(PacketDo        _) = negotiate nvt packet
step nvt packet@(PacketWont      _) = negotiate nvt packet
step nvt packet@(PacketDont      _) = negotiate nvt packet
step nvt packet@(PacketSubOption _) = negotiate nvt packet
step nvt _                          = (nvt, [])


negotiate :: Nvt -> Packet -> (Nvt, [Packet])
negotiate nvt packet = (nvt', response) where
    optCode = optionCode packet
    newFlag = case packet of
        (PacketWill _) -> True
        (PacketDo   _) -> True
        (PacketWont _) -> False
        (PacketDont _) -> False

    nvt'       = maybe nvt update' (lookup' optCode newNvtOpts)
    update' o  = update optCode o nvt
    newNvtOpts = fromList
        [(rfc856_BINARY_TRANSMISSION, setBoolOpt newFlag),
         (rfc857_ECHO,                setBoolOpt newFlag),
         (rfc858_SUPPRESS_GOAHEAD,    setBoolOpt newFlag),
         (rfc1073_WINDOW_SIZE,        id),
         (rfc1091_TERMINAL_TYPE,      id)]
        <*> nvt

    response  = fromMaybe [] (lookup' optCode response')
    response' = fromList
        [(rfc856_BINARY_TRANSMISSION, responseBoolOpt newFlag packet),
         (rfc857_ECHO,                responseBoolOpt newFlag packet),
         (rfc858_SUPPRESS_GOAHEAD,    responseBoolOpt newFlag packet),
         (rfc1073_WINDOW_SIZE,        responseWindowSize packet),
         (rfc1091_TERMINAL_TYPE,      responseTermType packet)]
        <*> nvt


setBoolOpt :: Bool -> NvtOpt -> NvtOpt
setBoolOpt newFlag        (NvtOptBool   _) = NvtOptBool newFlag
setBoolOpt _       nvtOpt@(NvtOptAlways _) = nvtOpt


responseBoolOpt :: Bool -> Packet -> NvtOpt -> [Packet]
responseBoolOpt newFlag packet (NvtOptBool   _   )
    | newFlag         = [ack packet]
    | otherwise       = [nak packet]
responseBoolOpt newFlag packet (NvtOptAlways flag)
    | newFlag == flag = [ack packet]
    | newFlag /= flag = [nak packet]
responseBoolOpt _ _ _ = []


responseWindowSize :: Packet -> NvtOpt -> [Packet]
responseWindowSize packet (NvtOptPair (w, h)) = [ack packet, naws w h]
responseWindowSize _      _                   = []


responseTermType :: Packet -> NvtOpt -> [Packet]
responseTermType (PacketSubOption subOpt) (NvtOptString termType)
    | subOpt == [rfc1091_TERMINAL_TYPE, '\1'] =
        [PacketSubOption (rfc1091_TERMINAL_TYPE:'\0':termType)]
    | otherwise      = []
responseTermType _ _ = []
