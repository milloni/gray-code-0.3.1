-- | QuickCheck properties of Codec.Binary.Gray module.
module Codec.Binary.Gray_props where

import Test.QuickCheck
import qualified Codec.Binary.Gray.Bits as B
import qualified Codec.Binary.Gray.List as L

import Data.Bits (testBit, finiteBitSize, Bits, FiniteBits)
import Data.Function (on)
import Data.Word (Word)

---
--- Properties of list-based functions
---

prop_lists_num2bin_id_Int =
  label "fromList . toList == id [Int]" $
  forAll (arbitrary :: Gen Int) $ \i ->
      i == (L.fromList . L.toList $ i)

prop_lists_num2bin_id_Integer =
  label "fromList . toList == id [Integer+]" $
  let i = (arbitrary :: Gen (NonNegative Integer))
  in  forAll i (\(NonNegative i) -> i == (L.fromList . L.toList $ i))

prop_lists_correct_bits_Int =
  label "toList is correct [Int]" $
  forAll (arbitrary :: Gen Int) $ \i ->
      let bts = map (testBit i) [0..(finiteBitSize i)-1]
          padded = (L.toList i) ++ (repeat False)
      in  all id $ zipWith (==) bts padded

prop_lists_bin2gray_id =
  label "binary . gray == gray . binary == id" $
  forAll (listOf $ (arbitrary :: Gen Bool)) $ \bs ->
      bs == (L.binary . L.gray $ bs) &&
      bs == (L.gray . L.binary $ bs)

prop_lists_gray_succ_Integer =
  label "hamming x (x+1) == 1 [Integer+]" $
  let i = (arbitrary :: Gen (NonNegative Integer))
  in  forAll i $ \(NonNegative i) -> succ_test i

prop_lists_gray_succ_Int =
  label "hamming x (x+1) == 1 [Int]" $
  let i = (arbitrary :: Gen Int)
  in  forAll i succ_test

succ_test :: (Bits a, Num a) => a -> Bool
succ_test = \i ->
      let n2g = L.gray . L.toList
          g1 = n2g i
          g2 = n2g (i+1)
      in  hamming g1 g2 == 1

hamming :: [Bool] -> [Bool] -> Int
hamming xs ys = go 0 xs ys
  where
    go d [] [] = d
    go d [] ys = go d [False] ys  -- extension for different lengths
    go d xs [] = go d [False] xs
    go d (x:xs) (y:ys) =
        if x == y
           then go d xs ys
           else go (d+1) xs ys

---
--- Properties of functions for FiniteBits types
---
prop_bits_id = label "binary . gray == gray . binary == id" $
  forAll (arbitrary :: Gen Int) $ \i ->
      (B.binary . B.gray $ i) == i && (B.gray . B.binary $ i) == i

prop_bits_same_as_lists =
  label "bitsToBinary . gray == binaryToGray . bitsToBinary [Int]" $
  forAll (arbitrary :: Gen Int) $ \i ->
      (L.gray . L.toList $ i) == (L.toList . B.gray $ i)

prop_bits_gray_succ_Int = label "hamming x (x+1) == 1 [Int]" $
  forAll (arbitrary :: Gen Int) $ \i ->
      (hammingBits `on` B.gray) i (i+1) == 1

prop_bits_gray_succ_Word = label "hamming x (x+1) == 1 [Word]" $
  forAll (arbitrary :: Gen Word) $ \w ->
      (hammingBits `on` B.gray) w (w+1) == 1

prop_bits_gray_succ_Integer = label "hamming x (x+1) == 1 [Integer]" $
  forAll (arbitrary :: Gen (NonNegative Integer)) $ \(NonNegative i) ->
      (hammingBits `on` B.gray) i (i+1) == 1

hammingBits :: (Bits a, Num a) => a -> a -> Int
hammingBits = hamming `on` L.toList

---
--- Test groups
---

prop_lists = label "[Bool]" $
  prop_lists_num2bin_id_Int .&.
  prop_lists_num2bin_id_Integer .&.
  prop_lists_correct_bits_Int .&.
  prop_lists_bin2gray_id .&.
  prop_lists_gray_succ_Int .&.
  prop_lists_gray_succ_Integer

prop_bits = label "Bits" $
  prop_bits_id .&.
  prop_bits_same_as_lists .&.
  prop_bits_gray_succ_Int .&.
  prop_bits_gray_succ_Word

all_props =
  prop_lists .&. prop_bits