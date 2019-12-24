import Clash.Prelude

import Clash.Container.MultiPortBlockRam

multiPortBlockRam' 
    :: forall dom 
    .  HiddenClockResetEnable dom
    => Vec 2 (Signal dom (Unsigned 8))                     -- ^ Read ports
    -> Vec 2 (Signal dom (Bool, Unsigned 8, BitVector 32)) -- ^ Write ports
    -> Vec 2 (Signal dom (BitVector 32))                   -- ^ Read results
multiPortBlockRam' raddrs writes = multiPortBlockRam raddrs (map (fmap hack) writes)
    where
    hack :: (Bool, Unsigned 8, BitVector 32) -> Maybe (Unsigned 8, BitVector 32)
    hack (False, _, _) = Nothing
    hack (True,  a, d) = Just (a, d)

{-# ANN topEntity
  (Synthesize
    { t_name   = "ram"
    , t_inputs = [ 
          PortName "clk"
        , PortName "rst"

        , PortProduct "" [
              PortName "raddr0"
            , PortName "raddr1"
        ]
        , PortProduct "" [
            PortProduct "" [
                  PortName "wen0"
                , PortName "waddr0"
                , PortName "wdata0"
            ],
            PortProduct "" [
                  PortName "wen1"
                , PortName "waddr1"
                , PortName "wdata1"
            ]
        ]
        ]
    , t_output = PortProduct "" [
          PortName "rdata0"
        , PortName "rdata1"
        ]
    }) #-}
topEntity clk rst = withClockResetEnable @System clk rst enableGen multiPortBlockRam'

