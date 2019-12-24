import Clash.Prelude

import Clash.Arithmetic.CarryLookahead

{-# ANN topEntity
  (Synthesize
    { t_name   = "koggeStone"
    , t_inputs = [ 
          PortName "carry_in" 
        , PortName "x" 
        , PortName "y" 
        ]
    , t_output = PortProduct "" [
          PortName "carrys"
        , PortName "sum"
        ]
    }) #-}
topEntity = koggeStone

