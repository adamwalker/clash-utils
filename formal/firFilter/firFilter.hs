import Clash.Prelude

import Clash.DSP.FIRFilter

goldenFIR 
    :: HiddenClockResetEnable dom
    => Vec 8 (Signed 16)
    -> Signal dom Bool
    -> Signal dom (Signed 16)
    -> Signal dom (Signed 16)
goldenFIR coeffs = fir (*) (+) coeffs

transposedFIR 
    :: HiddenClockResetEnable dom
    => Vec 8 (Signed 16)
    -> Signal dom Bool
    -> Signal dom (Signed 16)
    -> Signal dom (Signed 16)
transposedFIR coeffs = firTransposed (const (liftA3 macRealReal)) (reverse coeffs)
    where
    macRealReal c i a = c * i + a

{-# ANN topEntity1
  (Synthesize
    { t_name   = "golden_fir"
    , t_inputs = [ 
          PortName "clk"
        , PortName "rst"
        , PortName "coeffs"
        , PortName "valid" 
        , PortName "data_in" 
        ]
    , t_output = PortName "data_out"
    }) #-}
topEntity1 clk rst = withClockResetEnable @System clk rst enableGen goldenFIR

{-# ANN topEntity2
  (Synthesize
    { t_name   = "transposed_fir"
    , t_inputs = [ 
          PortName "clk"
        , PortName "rst"
        , PortName "coeffs"
        , PortName "valid" 
        , PortName "data_in" 
        ]
    , t_output = PortName "data_out"
    }) #-}
topEntity2 clk rst = withClockResetEnable @System clk rst enableGen transposedFIR
