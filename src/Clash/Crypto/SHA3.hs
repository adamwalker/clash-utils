{-# LANGUAGE NoMonomorphismRestriction #-}

{-| A straightforward, unoptimised <https://en.wikipedia.org/wiki/SHA-3 SHA3> implementation. 

    TODO: test on more than one block
-}
module Clash.Crypto.SHA3 (
        SHA3State,
        theta, 
        rho, 
        pi,
        chi,
        iota,
        round,
        updateState,
        sha3,
        sha3Packed,
        sha3_224,
        sha3_256,
        sha3_384,
        sha3_512
    ) where

import Clash.Prelude hiding (pi, round)
import Data.Bifunctor
import Clash.Misc

-- | Row major 5x5 matrix of 64 bit values
type SHA3State = Vec 5 (Vec 5 (BitVector 64))

-- | Theta block permutation step
theta :: SHA3State -> SHA3State
theta rows = transpose $ zipWith (map . xor) toXor $ transpose rows
    where
    paritys :: Vec 5 (BitVector 64)
    paritys =  map (fold xor) (transpose rows)

    toXor   :: Vec 5 (BitVector 64)
    toXor   =  zipWith xor (rotateRightS paritys d1) (map (flip rotateL 1) $ rotateLeftS paritys d1) 

-- | Rho block permutation step
rho :: SHA3State -> SHA3State
rho = unconcatI . zipWith (flip rotateL) rots . concat
    where
    rots
        =   0 :>  1 :> 62 :> 28 :> 27 
        :> 36 :> 44 :>  6 :> 55 :> 20 
        :>  3 :> 10 :> 43 :> 25 :> 39 
        :> 41 :> 45 :> 15 :> 21 :>  8 
        :> 18 :>  2 :> 61 :> 56 :> 14
        :> Nil

-- | Pi block permutation step
pi :: SHA3State -> SHA3State
pi rows = unconcatI $ map (concat rows !!) order
    where
    order 
        =  0 :> 6 :> 12 :> 18 :> 24
        :> 3 :> 9 :> 10 :> 16 :> 22
        :> 1 :> 7 :> 13 :> 19 :> 20
        :> 4 :> 5 :> 11 :> 17 :> 23
        :> 2 :> 8 :> 14 :> 15 :> 21
        :> Nil

-- | Chi block permutation step
chi :: SHA3State -> SHA3State
chi rows = transpose $ zipWith3 (zipWith3 func) cols (rotateLeftS cols d1) (rotateLeftS cols d2)
    where
    cols = transpose rows
    func :: BitVector 64 -> BitVector 64 -> BitVector 64 -> BitVector 64
    func x y z = x `xor` (complement y .&. z)

-- | Iota block permutation step
iota :: Index 24 -> SHA3State -> SHA3State
iota i ((x :> rest0) :> rest1) = ((x `xor` consts !! i) :> rest0) :> rest1
    where
    --TODO generate with LFSR
    consts 
        =  0x0000000000000001 :> 0x0000000000008082
        :> 0x800000000000808a :> 0x8000000080008000
        :> 0x000000000000808b :> 0x0000000080000001
        :> 0x8000000080008081 :> 0x8000000000008009
        :> 0x000000000000008a :> 0x0000000000000088
        :> 0x0000000080008009 :> 0x000000008000000a
        :> 0x000000008000808b :> 0x800000000000008b
        :> 0x8000000000008089 :> 0x8000000000008003
        :> 0x8000000000008002 :> 0x8000000000000080
        :> 0x000000000000800a :> 0x800000008000000a
        :> 0x8000000080008081 :> 0x8000000000008080
        :> 0x0000000080000001 :> 0x8000000080008008
        :> Nil

-- | Block permutation round
round :: Index 24 -> SHA3State -> SHA3State
round i = iota i . chi . pi . rho . theta

-- | Xor the data to be hashed into the block
updateState :: ((n + n0) ~ 25, KnownNat n0) => Vec n (BitVector 64) -> SHA3State -> SHA3State
updateState dat state = unconcatI $ zipWith xor (concat state) (dat ++ repeat 0)

-- | SHA3
sha3 
    :: forall dom n n0
    .  (HiddenClockResetEnable dom, (n + n0) ~ 25, KnownNat n0)
    => Signal dom Bool                         -- ^ Reset
    -> Signal dom (Vec n (BitVector 64))       -- ^ Input block
    -> (Signal dom Bool, Signal dom SHA3State) -- ^ (Done, hash state)
sha3 reset dat = (register False $ cnt .==. pure maxBound, state)
    where

    cnt :: Signal dom (Index 24)
    cnt =  register 0 $ step <$> cnt <*> reset
        where
        step 0   False = 0
        step cnt _     
            | cnt == maxBound = 0
            | otherwise       = cnt + 1

    roundsIn :: Signal dom SHA3State
    roundsIn =  mux (cnt .==. 0) (updateState <$> dat <*> state) state

    state :: Signal dom SHA3State
    state 
        = register (repeat (repeat 0)) 
        $ round <$> cnt <*> roundsIn

sha3Packed  
    :: forall inputSize outputSize rate dom drop 
    .  (HiddenClockResetEnable dom, 1600 ~ (outputSize + drop), 25 ~ (inputSize + rate), KnownNat rate, KnownNat outputSize)
    => Signal dom Bool 
    -> Signal dom (Vec inputSize (BitVector 64)) 
    -> (Signal dom Bool, Signal dom (BitVector outputSize))
sha3Packed start dat = second (fmap $ revBV . truncateB . revBV . pack . map swapEndian . concat) $ sha3 start dat

sha3_224 = sha3Packed @18 @224
sha3_256 = sha3Packed @17 @256
sha3_384 = sha3Packed @13 @384
sha3_512 = sha3Packed @9  @512

