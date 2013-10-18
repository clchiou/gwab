-- Copyright (C) 2013 Che-Liang Chiou.

module Telnet.Consts where

import qualified Data.Map as Map (Map, fromList)


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


rfc856_BINARY_TRANSMISSION = '\0'  :: OptionCode
rfc857_ECHO                = '\1'  :: OptionCode
rfc858_SUPPRESS_GOAHEAD    = '\3'  :: OptionCode
rfc1073_WINDOW_SIZE        = '\31' :: OptionCode
rfc1091_TERMINAL_TYPE      = '\24' :: OptionCode


makeNvtContext :: a -> NvtContext a
makeNvtContext a = fromList [
    (rfc856_BINARY_TRANSMISSION, a),
    (rfc857_ECHO,                a),
    (rfc858_SUPPRESS_GOAHEAD,    a),
    (rfc1073_WINDOW_SIZE,        a),
    (rfc1091_TERMINAL_TYPE,      a)]


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


data NvtOpt = NvtOptBool   { nvtOptBool   :: Bool       }
            | NvtOptAlways { nvtOptAlways :: Bool       }
            | NvtOptPair   { nvtOptPair   :: (Int, Int) }
            | NvtOptString { nvtOptString :: String     }
            | NvtOptNothing
              deriving (Eq, Show)


type    Nvt          = NvtContext NvtOpt
newtype NvtContext a = NvtContext {
    nvtContext :: Map.Map OptionCode a
} deriving (Eq)


fromList :: [(OptionCode, a)] -> NvtContext a
fromList = NvtContext . Map.fromList
