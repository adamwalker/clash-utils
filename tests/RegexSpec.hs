module RegexSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, fromList, sampleN_lazy, System)
import Data.Word
import qualified Data.ByteString as BS
import Test.Hspec
import Test.QuickCheck
import Text.Regex.TDFA hiding (Regex)
import Control.Applicative

import Clash.Regex.RegexCompile
import Clash.Regex.Regex

spec = describe "Regex matching" $ do
    specify "match"    $ property $ withMaxSuccess 1000 prop_match
    specify "no match" $ property $ withMaxSuccess 1000 prop_nomatch

--Compiled regular expression
resultHW :: CompiledRegexHW 6 8
resultHW = $(
        let 
            convert = toEnum . fromEnum
            a       = convert 'a' :: Word8
            b       = convert 'b'
            c       = convert 'c'
            h       = convert 'h'
            regex   = Concat (Concat (Concat (Star (Class [b])) (Concat (Class [c]) (Star ((Class [a]) `Union` (Class [b]))))) (Class [a, c])) (Class [h])
        in  regexTH $ compileRegex regex
    )

--Given a regex, generate an arbitrary string that matches it
genString :: Regex a -> Gen [a]
genString (Class  chars) = pure <$> elements chars
genString (Star   regex) = concat <$> listOf (genString regex)
genString (Concat x y)   = liftA2 (++) (genString x) (genString y)
genString (Union  x y)   = oneof [genString x, genString y]

--Repeat everything to get around the absurd template haskell "stage restriction"
convert = toEnum . fromEnum
a       = convert 'a' :: Word8
b       = convert 'b'
c       = convert 'c'
h       = convert 'h'
regex   = Concat (Concat (Concat (Star (Class [b])) (Concat (Class [c]) (Star ((Class [a]) `Union` (Class [b]))))) (Class [a, c])) (Class [h])

--Test matching strings
prop_match = forAll (genString regex) $ \str -> 
    last $ sampleN_lazy @System (length str + 3) 
         $ regexMatchBlockRam resultHW (pure True) 
         $ fromList ([0] ++ map pack str ++ repeat 0)

--Test strings that probably don't match
prop_nomatch str = res === expect
    where
    expect = BS.pack str =~ "\\`b*c(a|b)*[ac]h\\'"
    res = last 
        $ sampleN_lazy @System (length str + 3) 
        $ regexMatchBlockRam resultHW (pure True) 
        $ fromList ([0] ++ map pack str ++ repeat 0)

