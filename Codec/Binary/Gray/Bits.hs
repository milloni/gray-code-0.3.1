-- | Gray code is a binary numeral system where two successive numbers
-- differ in only one bit.
--
-- This module provides an interface to encode/decode @'Bits'@ types.
--
-- Algorithm:
--   Haupt, R.L. and Haupt, S.E., Practical Genetic Algorithms,
--   Second ed. (2004),  5.4. Gray Codes.
module Codec.Binary.Gray.Bits
    ( gray
    , binary
    , showBits
    ) where

import Data.Bits
    ( Bits, testBit, setBit, clearBit, finiteBitSize, bitSizeMaybe
    , shiftL, shiftR, complement, xor, (.&.), (.|.), isSigned)

import qualified Codec.Binary.Gray.List as L

-- | Right shift without extension of the sign bit (reset it to zero).
--
-- Results on negative values of unbounded integral types (like 'Integer') may be wrong.
shiftR' :: (Bits a, Num a) => a -> Int -> a
shiftR' n s =
  case (bitSizeMaybe n, signum n == (-1)) of
    (Just sz, True) ->
        let n' = clearBit (shiftR n 1) (sz - 1)
        in  shiftR' n' (s-1)
    (_, _) ->
        shiftR n s


-- | Convert an integer number from binary to Gray code.
--
-- Results on negative values of unbounded integral types (like 'Integer') may be wrong.
gray :: (Bits a, Num a) => a -> a
gray n = n `xor` (shiftR' n 1)

-- | Convert an integer number from Gray code to binary.
--
-- Results on negative values of unbounded integral types (like 'Integer') may be wrong.
binary :: (Bits a, Num a) => a -> a
binary 0 = 0
binary n =
    case maybeSz of
      (Just sz) ->
          let lastbit = sz - 1
              mask0 = let m = setBit 0 lastbit in (m, m)
              copyMSB n = (setBit 0 lastbit) .&. n
          in  binary' lastbit mask0 n (copyMSB n)
      Nothing ->  -- unbounded and negative
          0
  where
    maybeSz = case bitSizeMaybe n of
                (Just bsz) -> Just bsz
                Nothing -> effectiveBitSize n


effectiveBitSize :: (Bits a, Num a) => a -> Maybe Int
effectiveBitSize n
    | signum n == (-1) = bitSizeMaybe n
    | otherwise        = Just $ ebs n 0
  where
    ebs n bsz
        | signum n /= 1 = bsz
        | otherwise     = ebs (n `shiftR` 1) (bsz + 1)


binary' lastbit (maskReady, maskLast) ngray nbin
  | (maskReady .&. 1) /= 0 = nbin
  | otherwise =
     let
       nReady = maskReady .&. nbin
       maskReady' = setBit (shiftR maskReady 1) lastbit
       maskLast' = shiftR' maskLast 1
       nNext = (shiftR' (maskLast .&. nReady) 1) `xor` (maskLast' .&. ngray)
     in
       binary' lastbit (maskReady', maskLast') ngray (nReady .|. nNext)

-- | Render binary code as a string of @0@s and @1@s.
-- For example, @(42::Int8)@ is formatted as @101010@.
showBits :: (Bits a, Num a) => a -> String
showBits = L.showBits . L.toList
