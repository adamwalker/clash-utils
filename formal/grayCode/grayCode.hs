import Clash.Prelude

import Clash.GrayCode

{-# ANN topEntity1
  (Synthesize
    { t_name   = "binary_to_gray"
    , t_inputs = [ 
          PortName "binary_in" 
        ]
    , t_output = PortName "gray_out"
    }) #-}
topEntity1 :: BitVector 8 -> BitVector 8
topEntity1 =  binaryToGray

{-# ANN topEntity2
  (Synthesize
    { t_name   = "gray_to_binary"
    , t_inputs = [ 
          PortName "gray_in" 
        ]
    , t_output = PortName "binary_out"
    }) #-}
topEntity2 :: BitVector 8 -> BitVector 8
topEntity2 =  grayToBinary

