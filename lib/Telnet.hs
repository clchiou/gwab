-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet (
    Packet(..),
    parse,
    serialize,

    NvtContext(..),
    NvtOpt(..),
    Nvt,
    step,
) where

import Control.Applicative
import Control.Monad

import StringFilter
import StringFilter.Utils

import Telnet.Consts
import Telnet.Internal.Utils


--
-- Telnet : Wire Protocol
--


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


--
-- Telnet : Network Virtual Terminal
--


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
    nvt0 == nvt1 = foldl1N (&&) (liftA2 (==) nvt0 nvt1)


instance Show a => Show (NvtContext a) where
    show nvt = "NvtContext {" ++ opts ++ "}"
        where opts        = foldl1N joinopt opts'
              opts'       = showopt <$> nvtOptionName <*> nvt
              showopt n v = n ++ " = " ++ show v
              joinopt a b = a ++ ", " ++ b


nvtOptionName :: NvtContext String
nvtOptionName  = NvtContext {
    binary     = "binary",
    echo       = "echo",
    supGoAhead = "supGoAhead"
}


step :: Nvt -> Packet -> (Nvt, Maybe Packet)
step nvt packet@(PacketWill _) = negotiate nvt packet True
step nvt packet@(PacketDo   _) = negotiate nvt packet True
step nvt packet@(PacketWont _) = negotiate nvt packet False
step nvt packet@(PacketDont _) = negotiate nvt packet False
step nvt _                     = (nvt, Nothing)


negotiate :: Nvt -> Packet -> Bool -> (Nvt, Maybe Packet)
negotiate nvt packet flag = (nvt', Just response)
    where matched   = liftA2 (==) nvtOptionCode $ pure (optionCode packet)
          supported = fmap (/= NvtOptNothing) nvt
          -- Compute new nvt state
          nvt'      = select matched flag' nvt
          flag'     = pure (NvtOptBool flag)
          -- Compute response packet
          response  = pick nak matched makepkt packet
          makepkt   = select supported (pure ack) (pure nak)


nvtOptionCode :: NvtContext OptionCode
nvtOptionCode  = NvtContext {
    binary     = rfc856_BINARY_TRANSMISSION,
    echo       = rfc857_ECHO,
    supGoAhead = rfc858_SUPPRESS_GOAHEAD
}


select :: NvtContext Bool -> NvtContext a -> NvtContext a -> NvtContext a
select pred then_ else_ = liftA3 select' pred then_ else_
    where select' pred then_ else_ = if pred then then_ else else_


pick :: a -> NvtContext Bool -> NvtContext a -> a
pick zero pred candidate = foldlN pick' zero (liftA2 (,) pred candidate)
    where pick' z (p, c) = if p then c else z
