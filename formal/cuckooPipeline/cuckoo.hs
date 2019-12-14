import Clash.Prelude

import Data.Maybe

import Clash.Container.CuckooPipeline
import Clash.ErrorControl.CRC

hashtable
    :: HiddenClockResetEnable dom
    => Signal dom (BitVector 8)  -- ^ Key to lookup/modify/delete
    -> Signal dom Bool           -- ^ Perform modification
    -> Signal dom Bool           -- ^ Modification is delete
    -> Signal dom (BitVector 8)  -- ^ New value for modifications
    -> (
        Signal dom Bool,
        Signal dom (BitVector 8),
        Signal dom Bool
        )                        -- ^ (Lookup valid, Lookup result, busy)
hashtable lu modify delete val = (isJust <$> res, fromJust <$> res, busy)
    where
    (res, busy) = cuckooPipelineInsert hashFunctions lu (cmd <$> modify <*> delete <*> val)
    cmd modify delete val
        | modify && delete = Just Nothing
        | modify           = Just $ Just val
        | otherwise        = Nothing
    hashFunctions 
        =  (\x -> unpack (crc0 x))
        :> (\x -> unpack (crc1 x))
        :> Nil
        where
        table0 = $(lift $ (makeCRCTable (pack . crcSteps 0x33 (repeat 0)) :: Vec 8 (BitVector 6))) 
        crc0  = crcTable table0
        table1 = $(lift $ (makeCRCTable (pack . crcSteps 0x21 (repeat 0)) :: Vec 8 (BitVector 6))) 
        crc1  = crcTable table1 

{-# ANN topEntity
  (Synthesize
    { t_name   = "cuckoo"
    , t_inputs = [ 
          PortName "clk"
        , PortName "rst"
        , PortName "key" 
        , PortName "mod" 
        , PortName "delete" 
        , PortName "val" 
        ]
    , t_output = PortProduct "" [
          PortName "valid"
        , PortName "res"
        , PortName "busy"
        ]
    }) #-}
topEntity clk rst = withClockResetEnable @System clk rst enableGen hashtable

