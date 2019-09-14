module Clash.Network.Types where

import Clash.Prelude

data EthernetHeader = EthernetHeader {
    destMac   :: Vec 6 (BitVector 8),
    sourceMac :: Vec 6 (BitVector 8),
    etherType :: BitVector 16
} deriving (Generic, BitPack, Show, ShowX)

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
} deriving (Generic, BitPack, Show, ShowX)

data UDPHeader = UDPHeader {
    sourcePort  :: BitVector 16,
    destPort    :: BitVector 16,
    udpLen      :: BitVector 16,
    udpChecksum :: BitVector 16
} deriving (Generic, BitPack, Show, ShowX)

