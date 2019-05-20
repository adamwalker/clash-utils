{-# LANGUAGE TemplateHaskell #-}

{-| Regular expression matching as described in the paper "Compact Architecture for High-Throughput Regular Expression Matching on FPGA" -}
module Clash.Regex.RegexCompile (
    Regex(..),
    Matcher(..),
    CompiledRegex(..),
    compileRegex,
    CompiledRegexHW(..),
    regexTH
    ) where

import Prelude
import Clash.Prelude (Vec(..), type (+), type(^),  listToVecTH)
import Language.Haskell.TH
import Data.List

-- | Regex type parameterised by character type
data Regex a
    = Class  [a]
    | Star   (Regex a) 
    | Concat (Regex a) (Regex a)
    | Union  (Regex a) (Regex a)
    deriving (Show)

-- | Matching unit responsible for a single character class in the regex
data Matcher a = Matcher {
    chars    :: [a],
    incoming :: [Int]
} deriving (Show)

-- | Result of regex compilation 
data CompiledRegex a = CompiledRegex {
    matchers :: [Matcher a],
    size     :: Int,
    outputs  :: [Int]
} deriving (Show)

-- | Compile regex for hardware
compileRegex 
    :: Regex a         -- ^ `Regex` to compile
    -> CompiledRegex a -- ^ Compiled `Regex`
compileRegex regex = compileRegex' 0 regex [-1]
    where
    compileRegex' :: Int -> Regex a -> [Int] -> CompiledRegex a
    compileRegex' index (Class chars) incoming = CompiledRegex [Matcher chars incoming] 1 [index]
    compileRegex' index (Star  regex) incoming = CompiledRegex matchers size union
        where
        union                               = incoming ++ outputs
        CompiledRegex matchers size outputs = compileRegex' index regex union
    compileRegex' index (Concat regexX regexY) incoming = CompiledRegex (matchersX ++ matchersY) (sizeX + sizeY) outputsY
        where
        CompiledRegex matchersX sizeX outputsX = compileRegex' index           regexX incoming
        CompiledRegex matchersY sizeY outputsY = compileRegex' (index + sizeX) regexY outputsX
    compileRegex' index (Union regexX regexY) incoming = CompiledRegex (matchersX ++ matchersY) (sizeX + sizeY) (outputsX ++ outputsY)
        where
        CompiledRegex matchersX sizeX outputsX = compileRegex' index           regexX incoming
        CompiledRegex matchersY sizeY outputsY = compileRegex' (index + sizeX) regexY incoming

-- | A compiled regex suitable for hardware
data CompiledRegexHW n nBits = CompiledRegexHW {
    classesHW  :: Vec (2 ^ nBits) (Vec n Bool),
    matchersHW :: Vec n (Vec (n + 1) Bool),
    outputsHW  :: Vec (n + 1) Bool
}

listToVecTH2 :: [ExpQ] -> ExpQ
listToVecTH2 []     = [| Nil |]
listToVecTH2 (x:xs) = [| $(x) :> $(listToVecTH2 xs) |]

-- | Generate a `CompiledRegexHW` at compile time using template haskell
regexTH 
    :: (Eq a, Bounded a, Enum a) 
    => CompiledRegex a -- ^ Compiled regex
    -> Q Exp           -- ^ Template haskell AST for Regex
regexTH res = [| CompiledRegexHW $(listToVecTH2 $ map listToVecTH cs) $(listToVecTH2 $ map listToVecTH ms) $(listToVecTH os) |]
    where
    cs :: [[Bool]]
    cs =  transpose $ map (\matcher -> map (`elem` chars matcher) [minBound .. maxBound]) (matchers res)

    ms :: [[Bool]]
    ms =  map (\matcher -> map (`elem` incoming matcher) [(-1)..size res - 1]) (matchers res)

    os :: [Bool]
    os =  map (`elem` outputs res) [(-1)..size res - 1]

