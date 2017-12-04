{-
Tests for the compression library and command line tool
-}

module Tests where

import Test.HUnit
import Test.QuickCheck
import Compress
import Decompress
import Control.Monad (liftM2)
import Data.Monoid
import qualified Data.Word as W
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B

{-
Questions:
1. How to remove file extension?
2. Help with test prop_loseless and backslashes
3. How to optimize compress (ex. hard.txt)
-}

-- Unit tests for compressing small strings
tCompress :: Test
tCompress = "tCompress" ~: TestList
  [ compress "" ~?= BB.toLazyByteString (BB.word8 $ fromIntegral (-1))
  , compress "a" ~?= BB.toLazyByteString (BB.word16BE 97 <> BB.word8 (fromIntegral (-1)))
  , compress "aaa" ~?= BB.toLazyByteString (BB.word16BE 97 <> BB.word16BE 257 <> BB.word8 (fromIntegral (-1)))
  ]

-- Unit tests for decompressing small bytestrings
tDecompress :: Test
tDecompress = "tDecompress" ~: TestList
  [ decompress (BB.toLazyByteString (BB.word8 $ fromIntegral (-1))) ~?= ""
  , decompress (BB.toLazyByteString (BB.word16BE 97 <> BB.word8 (fromIntegral (-1)))) ~?= "a"
  , decompress (BB.toLazyByteString (BB.word16BE 97 <> BB.word16BE 257 <> BB.word8 (fromIntegral (-1)))) ~?= "aaa"
  ]

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

-- All the tests in one convenient place:

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

main :: IO ()
main = do
    putStrLn "Unit tests:"
    runTestTT $ TestList [tCompress, tDecompress]
    putStrLn "Quickcheck properties:"
    quickCheckN 500 prop_loseless
    quickCheckN 500 prop_ratio
