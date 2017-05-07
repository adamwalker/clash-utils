module CLaSH.Hamming where

import qualified Prelude as P
import qualified Data.List as L
import CLaSH.Prelude

--generator matrix for the parity bits
generator :: [[Bool]]
generator = L.transpose $ P.map generatorRow [0..]
    where

    generatorRow :: Int -> [Bool]
    generatorRow parityIndex = L.map (flip testBit parityIndex) nonPows

    nonPows :: [Int]
    nonPows = filter notPow2 [1..]
        where
        notPow2 :: Int -> Bool
        notPow2 n = n .&. (n - 1) /= 0

hammingParity
    :: forall m n. (KnownNat m, KnownNat n)
    => Vec (m + 1) (BitVector n) 
    -> BitVector (m + 1)        
    -> BitVector n             
hammingParity table input = fold xor $ zipWith func (unpack input) table
    where
    func     :: Bit -> BitVector n -> BitVector n
    func x   =  pack . map (.&. x) . unpack 

--TODO: there is probably a smarter way of doing this
correctError 
    :: forall n. (KnownNat n)
    => Vec n (BitVector (CLog 2 n))
    -> BitVector (CLog 2 n)
    -> BitVector n
    -> BitVector n
correctError nonPows parity dat = pack $ zipWith (func parity) nonPows (unpack dat)
    where 
    func :: BitVector (CLog 2 n) -> BitVector (CLog 2 n) -> Bool -> Bool
    func parity thisIdx dat
        | parity == thisIdx = not dat
        | otherwise         = dat

