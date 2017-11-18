{-
Tests for the compression library and command line tool
-}

module Tests where

import Test.HUnit
import Test.QuickCheck

{-
Questions:
1. How do we handle endianess?
2. What are more quickCheck properties we may be missing to test?
3. Are there other tests in general we should include?
-}

-- Unit tests for compressing small strings

-- Unit tests for decompressing small bytestrings

-- Test that (compression -> decompression) is lossless
prop_loseless :: String -> Bool

-- Test with small alphabet and many repeats, compressed file is smaller
prop_ratio :: String -> Bool

-- Informal testing of command line tool
