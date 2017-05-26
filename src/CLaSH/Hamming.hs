{-| Hamming code encoding and decoding. https://en.wikipedia.org/wiki/Hamming_code. -}
module CLaSH.Hamming where

import qualified Prelude as P
import qualified Data.List as L
import CLaSH.Prelude

-- | Generator matrix for the Hamming parity bits. Intended for use from template Haskell to ensure the matrix is generated a compile time. It's a lazy list of lazy lists, so use `Prelude.take` to make a generator matrix of the desired size. See the tests for an example.
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

-- | Calculate hamming parity bits
hammingParity
    :: forall m n. (KnownNat m, KnownNat n)
    => Vec (m + 1) (BitVector n) -- ^ Parity bits generator matrix
    -> BitVector (m + 1)         -- ^ Input vector
    -> BitVector n               -- ^ Parity bits
hammingParity table input = fold xor $ zipWith func (unpack input) table
    where
    func     :: Bit -> BitVector n -> BitVector n
    func x   =  pack . map (.&. x) . unpack 

--TODO: there is probably a smarter way of doing this
-- | Correct a single bit error in a hamming code word
correctError 
    :: forall n. (KnownNat n)
    => Vec n (BitVector (CLog 2 n)) -- ^ Parity bits generator matrix
    -> BitVector (CLog 2 n)         -- ^ Error parity bits
    -> BitVector n                  -- ^ Data bits
    -> BitVector n                  -- ^ Corrected data word
correctError nonPows parity dat = pack $ zipWith (func parity) nonPows (unpack dat)
    where 
    func :: BitVector (CLog 2 n) -> BitVector (CLog 2 n) -> Bool -> Bool
    func parity thisIdx dat
        | parity == thisIdx = not dat
        | otherwise         = dat

