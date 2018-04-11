module CRCSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset, lift)
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck

import Data.Word
import Data.Digest.CRC32
import Clash.CRC
import Clash.Misc

--Check that the CRC implementation agrees with a known goood implementation of CRC32
spec = describe "CRC" $ do
    specify "matches known good implementation"      $ property prop_crc32
    specify "equals crc verification of zero padded" $ property prop_crc32_2
    specify "verification"                           $ property prop_crc32_verify
    specify "table based implementation"             $ property prop_crc32_table
    specify "table based implementation of verify"   $ property prop_crc32_table_verify
    specify "compute in multiple steps"              $ property prop_crc32_multistep
    specify "compute in multiple steps 2"            $ property prop_crc32_multistep_2
    specify "compute verify in multiple steps"       $ property prop_crc32_multistep_verify
    
reverseByte :: Word8 -> Word8
reverseByte = unpack . revBV . pack

toBytes :: forall n. KnownNat n => BitVector (n * 8) -> [Word8]
toBytes x = map (fromIntegral . pack) $ Clash.toList unpacked
    where
    unpacked :: Vec n (Vec 8 Bit)
    unpacked = Clash.unconcatI (unpack x)

prop_crc32 :: BitVector 128 -> Bool
prop_crc32 x = result == expect
    where
    expect = crc32 $ map reverseByte (toBytes x)
    result = fromIntegral $ pack $ Clash.map Clash.complement $ Clash.reverse $ crcSteps crc32Poly (Clash.repeat 1) x

prop_crc32_2 :: BitVector 128 -> Bool
prop_crc32_2 x = result == expect
    where
    expect = crcSteps       crc32Poly (Clash.repeat 0) x
    result = crcVerifySteps crc32Poly (Clash.repeat 0) $ x ++# (0 :: BitVector 32)

prop_crc32_verify :: BitVector 128 -> Bool
prop_crc32_verify x = result == 0
    where
    checksum = pack $ crcSteps       crc32Poly (Clash.repeat 0) x
    result   = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) $ x ++# checksum

prop_crc32_table :: BitVector 128 -> Bool
prop_crc32_table x = result == expect
    where
    expect = pack $ crcSteps crc32Poly (Clash.repeat 0) x
    result = crcTable (makeCRCTable (pack . crcSteps crc32Poly (Clash.repeat 0))) x

prop_crc32_table_verify :: BitVector 128 -> Bool
prop_crc32_table_verify x = result == expect
    where
    expect = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) x
    result = crcTable (makeCRCTable (pack . crcVerifySteps crc32Poly (Clash.repeat 0))) x

prop_crc32_multistep :: BitVector 256 -> Bool
prop_crc32_multistep x = unpack result == expect
    where
    expect = pack $ crcSteps crc32Poly (Clash.repeat 0) x
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcSteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = foldl step 0 words

prop_crc32_multistep_2 :: BitVector 256 -> Bool
prop_crc32_multistep_2 x = unpack result == expect
    where
    expect = crc32 $ Prelude.map reverseByte (toBytes x)
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcSteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = fromIntegral $ pack $ Clash.map Clash.complement $ Clash.reverse $ (unpack $ foldl step 0xffffffff words :: Vec 32 Bit)

prop_crc32_multistep_verify :: BitVector 256 -> Bool
prop_crc32_multistep_verify x = unpack result == expect
    where
    expect = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) x
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcVerifySteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = foldl step 0 words

prop_crc32_table_th :: BitVector 128 -> Bool
prop_crc32_table_th x = result == expect
    where
    expect = pack $ crcSteps crc32Poly (Clash.repeat 0) x
    result = crcTable $(lift $ (makeCRCTable (pack . crcSteps crc32Poly (Clash.repeat 0)) :: Vec 128 (BitVector 32))) x

