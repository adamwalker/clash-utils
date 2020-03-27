{-# LANGUAGE DuplicateRecordFields #-}
-- | Network header types
module Clash.Network.Types where

import Clash.Prelude

-- | [Ethernet header](https://en.wikipedia.org/wiki/Ethernet_frame)
data EthernetHeader = EthernetHeader {
    destMac   :: Vec 6 (BitVector 8),
    sourceMac :: Vec 6 (BitVector 8),
    etherType :: BitVector 16
} deriving (Generic, BitPack, Show, ShowX, Eq, Ord)

instance Default EthernetHeader where
    def = EthernetHeader {
        destMac   = repeat 0xff,
        sourceMac = repeat 0,
        etherType = 0x0800
    }

-- | [IPv4 header](https://en.wikipedia.org/wiki/IPv4)
data IPv4Header = IPv4Header {
    version        :: BitVector 4,
    ihl            :: BitVector 4,
    dscp           :: BitVector 6,
    ecn            :: BitVector 2,
    ipLen          :: BitVector 16,
    identification :: BitVector 16,
    flags          :: BitVector 3,
    fragmentOffset :: BitVector 13,
    ttl            :: BitVector 8,
    protocol       :: BitVector 8,
    headerChecksum :: BitVector 16,
    sourceIP       :: Vec 4 (BitVector 8),
    destIP         :: Vec 4 (BitVector 8)
} deriving (Generic, BitPack, Show, ShowX, Eq, Ord)

instance Default IPv4Header where 
    def = IPv4Header {
        version        = 4,
        ihl            = 5,
        dscp           = 0,
        ecn            = 0,
        ipLen          = 0,
        identification = 0,
        flags          = 0,
        fragmentOffset = 0,
        ttl            = 0x80,
        protocol       = 0,
        headerChecksum = 0,
        sourceIP       = repeat 0,
        destIP         = repeat 0
    }

-- | [UDP header](https://en.wikipedia.org/wiki/User_Datagram_Protocol)
data UDPHeader = UDPHeader {
    sourcePort  :: BitVector 16,
    destPort    :: BitVector 16,
    udpLen      :: BitVector 16,
    udpChecksum :: BitVector 16
} deriving (Generic, BitPack, Show, ShowX, Eq, Ord)

-- | [TCP header](https://en.wikipedia.org/wiki/Transmission_Control_Protocol)
data TCPHeader = TCPHeader {
    sourcePort    :: BitVector 16,
    destPort      :: BitVector 16,
    seqNum        :: BitVector 32,
    ackNum        :: BitVector 32,
    dataOffset    :: BitVector 4,
    reserved      :: BitVector 3,
    flags         :: BitVector 9,
    windowSize    :: BitVector 16,
    checksum      :: BitVector 16,
    urgentPointer :: BitVector 16
} deriving (Generic, BitPack, Show, ShowX, Eq, Ord)

