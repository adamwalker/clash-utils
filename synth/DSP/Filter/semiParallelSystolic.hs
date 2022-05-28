import Clash.Prelude
import Clash.Annotations.TH

import Clash.DSP.FIR.SemiParallel
import Clash.DSP.MAC

coeffs :: Vec 4 (Vec 16 (Signed 16))
coeffs = (
        (1230 :> 2121 :> 1120 :> 1120 :> 1231 :> 2120 :> 1120 :> 1121 :> 1230 :> 2121 :> 1120 :> 1120 :> 2120 :> 1120 :> 1121 :> 1230 :> Nil) :> 
        (2130 :> 2311 :> 1450 :> 1451 :> 2131 :> 2310 :> 1450 :> 1450 :> 2130 :> 2311 :> 1450 :> 1451 :> 2310 :> 1450 :> 1450 :> 2130 :> Nil) :> 
        (1231 :> 2120 :> 1120 :> 1121 :> 1230 :> 2121 :> 1120 :> 1120 :> 1231 :> 2120 :> 1120 :> 1121 :> 2121 :> 1120 :> 1120 :> 1231 :> Nil) :> 
        (2131 :> 2310 :> 1450 :> 1450 :> 2130 :> 2311 :> 1450 :> 1451 :> 2131 :> 2310 :> 1450 :> 1450 :> 2311 :> 1450 :> 1451 :> 2131 :> Nil) :> 
    Nil) 

theFilter 
    :: HiddenClockResetEnable dom
    => Signal dom Bool                                            -- ^ Input valid
    -> Signal dom (Signed 16)                                      -- ^ Sample
    -> (Signal dom Bool, Signal dom (Signed 40), Signal dom Bool) -- ^ (Output valid, output data, ready)
theFilter
    = semiParallelFIRSystolic 
        macRealRealPipelined 
        (SNat @2) 
        coeffs
        (pure 0)

top
    :: "clk"       ::: Clock System
    -> "iValid"    ::: Signal System Bool
    -> "iSampleIn" ::: Signal System (Signed 16)
    -> (
            "oVld"   ::: Signal System Bool,
            "oDat"   ::: Signal System (Signed 40),
            "oReady" ::: Signal System Bool
       )
top clk = exposeClockResetEnable theFilter clk resetGen enableGen

makeTopEntity 'top

