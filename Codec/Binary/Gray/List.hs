-- | Gray code is a binary numeral system where two successive numbers
-- differ in only one bit.
--
-- This module provides an interface to encode/decode numbers
-- represented as lists of @Bool@.
--
-- Algorithm:
--   Haupt, R.L. and Haupt, S.E., Practical Genetic Algorithms,
--   Second ed. (2004),  5.4. Gray Codes.
module Codec.Binary.Gray.List
    ( gray, binary
    , toList, toList', fromList
    , showBits
    ) where

import Data.Bits (FiniteBits, Bits, testBit, finiteBitSize, bitSizeMaybe, shiftR, isSigned)

boolXOR :: Bool -> Bool -> Bool
boolXOR p q = (p && not q) || (not p && q)

-- | Take a list of bits (most significant last) in binary encoding
-- and convert them to Gray code.
gray :: [Bool] -> [Bool]
gray (b:c:bs) = b `boolXOR` c : gray (c:bs)
gray [b] = [b]
gray [] = []

-- | Take a list of bits in Gray code and convert them to binary encoding
-- (most significant bit last).
binary :: [Bool] -> [Bool]
binary = foldr go []
  where go c [] = [c]
        go c bs@(b:_) = b `boolXOR` c : bs

-- | Convert a number to a list of bits in usual binary encoding (most
-- significant bit last). Truncates unset major bits.
--
-- The function may be also applied to unbounded integral types (like
-- 'Integer'): it will return a list of bits for positive values, and
-- an empty list for negative values or zero.
toList :: (Bits b, Num b) => b -> [Bool]
toList 0 = []
toList i =
  let mbSize = bitSizeMaybe i
      isNegative = isSigned i && signum i == (-1)
  in  case (mbSize, isNegative) of
        (Just _, False) -> positiveToList i
        (Just size, True) -> negativeToList size i
        (Nothing, False) -> positiveToList i
        (Nothing, True) -> []
  where
    positiveToList i =
      let rest = toList $ shiftR i 1  -- works only for positive i
      in  (testBit i 0 : rest)
    negativeToList bsize i =
        let b = map not . toList $ negate i - 1
        in  b ++ (take (bsize - length b) $ repeat True)
        --    ^^^ pad major bits

-- | Convert a number to a list of bits in usual binary encoding (most
-- significant bit last).
--
-- Like 'toList', but returns all unset major bits too. So the length
-- of the output is always the same length as @finiteBitSize i@.
toList' :: (FiniteBits b, Num b) => b -> [Bool]
toList' i = map (testBit i) [0..finiteBitSize i - 1]

-- | Convert a list of bits in binary encoding to a number.
fromList :: (Bits b, Num b) => [Bool] -> b
fromList = sum . map fst . filter snd . zip (map (2^) [0..])

-- | Render a list of bits as a string of @0@s and @1@s.
showBits :: [Bool] -> String
showBits [] = "0"
showBits bs = map (\b -> if b then '1' else '0') . reverse $ bs
