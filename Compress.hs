{-
A compression function
-}

module Compress where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W
import qualified Data.ByteString.Builder as BB
import Data.Sequence ((><), (<|), (|>))
-- Stores the encodings of a string to bits
type Encoding = M.Map String W.Word16

-- Max size of ecoding table
maxSize :: Int
maxSize = 2^16 - 2

-- Main compression function, takes in String
{-
1. input: String s
2. t = initTable
3. b = lzwCompress s t
4. return byte string of b
-}
compress :: String -> B.ByteString
compress s = BB.toLazyByteString $ lzwCompress s t where
  t :: Encoding
  t = initTable

-- Create the table with all possible single ascii characters
{-
1. Add all ascii to table with value [a..z,A..Z] key [0..255]
-}
initTable :: Encoding
initTable = foldr (\i e' -> M.insert [C.chr i]
  (fromIntegral i) e') e [0..255] where
    e :: Encoding
    e = M.empty

-- Compress the string into a bit string using LZW
{-
1. Pattern match on string to find match in encoding table
2. Append Match to ByteString
3. Add new encoding (from nextPattern) with code of: (size encoding)
4. Recursively call append (result of getting match in map) lzwCompress
5. Base case: -1
-}
lzwCompress :: String -> Encoding -> BB.Builder
lzwCompress s@(x:xs) e = BB.word16BE (e M.! match) <> lzwCompress s' e'
    where (match, s') = nextPattern [x] xs e
          e' = if null s' then e else
            addEncoding e (match ++ [head s']) (fromIntegral $ M.size e')
lzwCompress []    _ = BB.word8 $ fromIntegral (-1)

-- Get next largest pattern that is in the LZW table
{-
1. s0 s1 e -> (match, s')
-}
nextPattern :: String -> String -> Encoding -> (String, String)
nextPattern s0 s1@(x:xs) e
  | M.member test e = nextPattern test xs e
  | otherwise            = (s0,s1)
    where test = s0 ++ [x]
nextPattern s0 s1 e      = (s0,s1)

-- Add to Encoding safely (does that add more than the max table size)
{-
1. If (size e) > 2^16, return e
2. Otherwise, return (add e s w)
-}
addEncoding :: Encoding -> String -> W.Word16 -> Encoding
addEncoding e s w
  | M.size e > maxSize = e
  | otherwise          = M.insert s w e
