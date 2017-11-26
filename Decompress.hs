{-
A decompression function
-}

module Decompress where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W
import qualified Data.ByteString.Builder as BB
-- Stores the decodings of bits to string
type Decoding = M.Map W.Word16 String

-- Max size of ecoding table
maxSize :: Int
maxSize = 2^16

-- Main decompression function, takes in String
{-
1. input: ByteString bs
2. t = initTable
3. s = lzwDecompress bs 0 t
4. return b
-}
decompress :: B.ByteString -> String
decompress bs = lzwDecompress bs 0 t where
  d :: Decoding
  d = initTable

-- Create the table with all possible single ascii characters
{-
map[word16(0)] = ""
1. Add all ascii to table with key [0..256] value [a..z,A..Z]
-}
initTable :: Decoding
initTable = foldr (\i d' -> M.insert
  (fromIntegral i) [C.chr i] d') d [0..255] where
    d :: Decoding
    d = M.empty

-- Compress the string into a bit string using LZW
{-
1. Get next word16
2. Get nextDecoding with word16 and previous
3. Recursively call d[curr] :: lzwDecompress
4. Base case: if (Word16 0), then EOF so ""
-}
lzwDecompress :: B.ByteString -> W.Word16 -> Decoding -> String
lzwDecompress = undefined

-- Get next decoding from decoding table
{-
1. d w0 w1 = gets the string matching w0 and the string matching w1
2. creates new decoding of (size + 1) for s0 + s1[0]
3. Edge case: when w1 is not in the d then w1 = w0
-}
nextDecoding :: Decoding -> W.Word16 -> W.Word16 -> Decoding
nextDecoding d w0 w1
  | M.member w1 d = addDecoding d (fromIntegral $ M.size d)
      (s0 ++ [head s1])
  | otherwise     = undefined
      where s0 = d M.! w0
            s1 = d M.! w1

-- Add to Decoding safely (does that add more than the max table size)
{-
1. If (size e) > 2^16, return e
2. Otherwise, return (add e s w)
-}
addDecoding :: Decoding -> W.Word16 -> String -> Decoding
addDecoding d w s
  | M.size d > maxSize = d
  | otherwise          = M.insert w s d
