{-
Tests for the compression library and command line tool
-}

module Tests where

import Test.HUnit
import Test.QuickCheck
import Compress
import Decompress
import Control.Monad (liftM2)
import qualified Data.ByteString.Lazy as B

{-
Questions:
1. How to remove file extension?
2. Help with test prop_loseless and backslashes
3. How to optimize compress (ex. hard.txt)
-}

-- Unit tests for compressing small strings

-- Unit tests for decompressing small bytestrings

-- Test that (compression -> decompression) is lossless
prop_loseless :: String -> Property
prop_loseless s = '\\' `notElem` s ==> s == (decompress . compress) s

-- Test with small alphabet and many repeats, compressed file is smaller
prop_ratio :: SmallAlphabet -> Property
prop_ratio s = length (toString s) > 200 ==> lenBs <= lenS' where
  s' = toString s
  bs = compress s'
  lenS' = length s'
  lenBs = length (B.unpack bs)

newtype SmallAlphabet = SmallAlphabet String deriving (Eq, Ord, Show, Read)

instance Arbitrary SmallAlphabet where
  arbitrary  = SmallAlphabet <$> sized gen where
    gen n = frequency [(1, return []), (1000, liftM2 (++) (elements ["a","ab","aa","b","bb","ba"]) (gen 1000))]

toString :: SmallAlphabet -> String
toString (SmallAlphabet s) = s

-- Informal testing of command line tool
-- *Complete*
