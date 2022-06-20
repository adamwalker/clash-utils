import Clash.Prelude
import Clash.Annotations.TH

import Clash.DSP.MAC
import Clash.DSP.FIR.SemiParallel
import Clash.DSP.FIR.HalfBand

coeffs :: Vec 4 (Vec 16 (Signed 16))
coeffs = (
        (1230 :> 2121 :> 1120 :> 1120 :> 1231 :> 2120 :> 1120 :> 1121 :> 1230 :> 2121 :> 1120 :> 1120 :> 2120 :> 1120 :> 1121 :> 1230 :> Nil) :> 
        (2130 :> 2311 :> 1450 :> 1451 :> 2131 :> 2310 :> 1450 :> 1450 :> 2130 :> 2311 :> 1450 :> 1451 :> 2310 :> 1450 :> 1450 :> 2130 :> Nil) :> 
        (1231 :> 2120 :> 1120 :> 1121 :> 1230 :> 2121 :> 1120 :> 1120 :> 1231 :> 2120 :> 1120 :> 1121 :> 2121 :> 1120 :> 1120 :> 1231 :> Nil) :> 
        (2131 :> 2310 :> 1450 :> 1450 :> 2130 :> 2311 :> 1450 :> 1451 :> 2131 :> 2310 :> 1450 :> 1450 :> 2311 :> 1450 :> 1451 :> 2131 :> Nil) :> 
    Nil) 

filter' 
    :: forall dom. HiddenClockResetEnable dom 
    => Signal dom (Signed 40) 
    -> Signal dom Bool 
    -> Signal dom (Signed 16) 
    -> (Signal dom Bool, Signal dom (Signed 40), Signal dom Bool)
filter' 
    = semiParallelFIRSystolicSymmetric 
        macPreAddRealRealPipelined 
        (evenSymmAccum2 (SNat @2) macPreAddRealRealPipelined (last coeffs)) 
        (SNat @2) 
        (init coeffs)

theFilter 
    :: HiddenClockResetEnable dom
    => Signal dom Bool                                            -- ^ Input valid
    -> Signal dom (Signed 16)                                     -- ^ Sample
    -> (Signal dom Bool, Signal dom (Signed 40), Signal dom Bool) -- ^ (Output valid, output data, ready)
theFilter
    = halfBandDecimate 
        (SNat @16) 
        extend 
        filter'

top
    :: "clk"       ::: Clock XilinxSystem
    -> "iValid"    ::: Signal XilinxSystem Bool
    -> "iSampleIn" ::: Signal XilinxSystem (Signed 16)
    -> (
            "oVld"   ::: Signal XilinxSystem Bool,
            "oDat"   ::: Signal XilinxSystem (Signed 40),
            "oReady" ::: Signal XilinxSystem Bool
       )
top clk = exposeClockResetEnable theFilter clk resetGen enableGen

makeTopEntity 'top

