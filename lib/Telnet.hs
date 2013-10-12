-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet (
    StringFilter.Error(Err, NeedMoreInput),

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


parse :: String -> FilterResult Packet
parse input =
    case runFilter filterTelnet input of
        Right (packet, rest) -> Right (packet, rest)
        Left  (Err reason)   -> Left  (Err reason)
        Left  NeedMoreInput  -> Left  NeedMoreInput


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
        supGoAhead = f $ supGoAhead a,
        windowSize = f $ windowSize a,
        termType   = f $ termType   a
    }


instance Applicative NvtContext where
    pure v = NvtContext {
        binary     = v,
        echo       = v,
        supGoAhead = v,
        windowSize = v,
        termType   = v
    }

    f <*> a = NvtContext {
        binary     = binary     f (binary     a),
        echo       = echo       f (echo       a),
        supGoAhead = supGoAhead f (supGoAhead a),
        windowSize = windowSize f (windowSize a),
        termType   = termType   f (termType   a)
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
    supGoAhead = "supGoAhead",
    windowSize = "windowSize",
    termType   = "termType"
}


step :: Nvt -> Packet -> (Nvt, [Packet])
step nvt packet@(PacketWill      _) = negotiate nvt packet True
step nvt packet@(PacketDo        _) = negotiate nvt packet True
step nvt packet@(PacketWont      _) = negotiate nvt packet False
step nvt packet@(PacketDont      _) = negotiate nvt packet False
step nvt packet@(PacketSubOption _) = subnegotiate nvt packet
step nvt _                          = (nvt, [])


negotiate :: Nvt -> Packet -> Bool -> (Nvt, [Packet])
negotiate nvt packet flag = (nvt', response:extra)
    where opt       = optionCode packet
          matched   = liftA2 (==) nvtOptionCode $ pure opt
          supported = fmap (/= NvtOptNothing) nvt
          -- Compute new nvt state
          nvt'      = select (matched &&& negotiable) flag' nvt
          flag'     = pure (NvtOptBool flag)
          -- Compute response packet
          response  = (pick nak matched makepkt) packet
          makepkt   = select supported (pure ack) (pure nak)
          -- Compute extra response packets
          extra     = unJust $ reportWindowSize nvt packet
          -- Helper table
          nvtOptionCode  = NvtContext {
              binary     = rfc856_BINARY_TRANSMISSION,
              echo       = rfc857_ECHO,
              supGoAhead = rfc858_SUPPRESS_GOAHEAD,
              windowSize = rfc1073_WINDOW_SIZE,
              termType   = rfc1091_TERMINAL_TYPE
          }
          negotiable     = NvtContext {
              binary     = True,
              echo       = True,
              supGoAhead = True,
              windowSize = False,
              termType   = False
          }
          -- Helper function
          unJust (Just p) = [p]
          unJust Nothing  = []


reportWindowSize :: Nvt -> Packet -> Maybe Packet
reportWindowSize nvt (PacketDo opt)
    | opt == rfc1073_WINDOW_SIZE =
        case windowSize nvt of
            (NvtOptPair (w, h)) -> Just $ naws w h
            _                   -> Nothing
    | otherwise                  = Nothing
reportWindowSize _   _           = Nothing


subnegotiate :: Nvt -> Packet -> (Nvt, [Packet])
subnegotiate nvt packet
    | subOption packet /= [rfc1091_TERMINAL_TYPE, '\1'] = (nvt, [])
    | otherwise                                         =
        (nvt, [PacketSubOption (rfc1091_TERMINAL_TYPE:'\0':termType')])
            where termType' = nvtOptString $ termType nvt


select :: NvtContext Bool -> NvtContext a -> NvtContext a -> NvtContext a
select pred then_ else_ = liftA3 select' pred then_ else_
    where select' a b c = if a then b else c


pick :: a -> NvtContext Bool -> NvtContext a -> a
pick zero pred candidate = foldlN pick' zero (liftA2 (,) pred candidate)
    where pick' z (p, c) = if p then c else z


(&&&) :: NvtContext Bool -> NvtContext Bool -> NvtContext Bool
(&&&) = liftA2 (&&)
