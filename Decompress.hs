{-
A decompression function
-}

module Decompress where

import qualified Data.Map as M
-- Stores the decodings of bits to string
type Decoding = M.Map Word16 String

-- Main decompression function, takes in String
decompress :: ByteString -> String
{-
1. input: ByteString bs
2. t = initTable
3. s = lzwDecompress bs 0 t
4. return b
-}

-- Create the table with all possible single ascii characters
initTable :: String -> Decoding
{-
map[word16(0)] = ""
1. Add all ascii to table with key [0..256] value [a..z,A..Z]
-}

-- Compress the string into a bit string using LZW
lzwDecompress :: ByteString -> Word16 -> Decoding -> String
{-
1. Get next word16
2. Get nextDecoding with word16 and previous
3. Recursively call d[curr] :: lzwDecompress
4. Base case: if (Word16 0), then EOF so ""
-}

nextDecoding :: Decoding -> Word16 -> Word16 -> Decoding
{-
1. d p c = gets the string matching the p and the string matching c
2. creates new decoding of (size + h1) for p + c[0]
3. Edge case: when c is not in the d then c = the p
-}

-- Add to Decoding safely (does that add more than the max table size)
addDecoding :: Decoding -> Word16 -> String -> Decoding
{-
1. If (size e) > 2^16, return e
2. Otherwise, return (add e s w)
-}
