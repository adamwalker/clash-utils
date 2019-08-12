{-| Regular expression matching as described in the paper "Compact Architecture for High-Throughput Regular Expression Matching on FPGA" -}
module Clash.Regex.Regex (
    regexMatchComb,
    regexMatch,
    regexMatchBlockRam
    ) where

import Clash.Prelude

import Clash.Regex.RegexCompile

-- | Regular expression matching combinational logic
regexMatchComb
    :: forall dom n
    .  KnownNat n
    => Vec n (Vec (n + 1) Bool) -- ^ Matching engine connection matrix
    -> Bool                     -- ^ Matches may start here
    -> Vec n Bool               -- ^ Current state
    -> Vec n Bool               -- ^ Character class matches
    -> Vec n Bool               -- ^ Next state
regexMatchComb connections start states incoming = zipWith func incoming connections
    where
    func :: Bool -> Vec (n + 1) Bool -> Bool
    func incoming connections = incoming && fold (||) (zipWith (&&) connections (start :> states))

-- | Regular expression matching sequential logic
regexMatch
    :: forall dom n
    .  (HiddenClockResetEnable dom, KnownNat n)
    => Vec n (Vec (n + 1) Bool) -- ^ Matching engine connection matrix
    -> Signal dom Bool          -- ^ Matches may start here
    -> Signal dom (Vec n Bool)  -- ^ Character class matches
    -> Signal dom (Vec n Bool)  -- ^ Matching state
regexMatch connections start incoming = res 
    where
    res = register (repeat False) $ regexMatchComb connections <$> start <*> res <*> incoming

-- | Regular expression matching where the character classes are matched using block rams
regexMatchBlockRam 
    :: forall dom n m a
    .  (HiddenClockResetEnable dom, KnownNat n, BitPack a, m ~ BitSize a, KnownNat m)
    => CompiledRegexHW (n + 1) m -- ^ Result of regex compilation
    -> Signal dom Bool           -- ^ Matches may start here
    -> Signal dom a              -- ^ Input character
    -> Signal dom Bool           -- ^ Match
regexMatchBlockRam compiled start input = fmap reduce $ regexMatch (matchersHW compiled) start $ sequenceA romLookups
    where
    romLookups :: Vec (n + 1) (Signal dom Bool)
    romLookups =  sequenceA $ blockRam (classesHW compiled) (bitCoerce <$> input) (pure Nothing :: Signal dom (Maybe (Unsigned m, Vec (n + 1) Bool)))
    
    reduce :: Vec (n + 1) Bool -> Bool
    reduce results = fold (||) $ zipWith (&&) (tail $ outputsHW compiled) results

