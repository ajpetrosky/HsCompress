{-
A compression function
-}

module Compress where

import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.Word as W
-- Stores the encodings of a string to bits
type Encoding = M.Map String W.Word16

-- Max size of ecoding table
maxSize :: Int
maxSize = 2^16

-- Main compression function, takes in String
{-
1. input: String s
2. t = initTable
3. b = lzwCompress s t
4. return b
-}
compress :: String -> B.ByteString
compress s = lzwCompress s t where
  t :: Encoding
  t = initTable s

-- Create the table with all possible single ascii characters
{-
1. Add all ascii to table with value [a..z,A..Z] key [0..256]
-}
initTable :: String -> Encoding
initTable s = foldr (\i t' -> M.insert [C.chr i]
  (fromIntegral i) t') t [0..255] where
    t :: Encoding
    t = M.empty

-- Compress the string into a bit string using LZW
{-
1. Pattern match on string to find match in encoding table
2. CONS Match to ByteString
3. Add new encoding (from nextPattern) with code of: (size encoding) + 1
4. Recursively call append (result of getting match in map) lzwCompress
5. Return EOF bytes string
6. Base case: ByteString (Word16 0) (the first table entry)
-}
lzwCompress :: String -> Encoding -> B.ByteString
lzwCompress = undefined

-- Get next largest pattern that is in the LZW table
{-
1. s0 s1 e -> (match, s')
-}
nextPattern :: String -> String -> Encoding -> (String, String)
nextPattern = undefined

-- Add to Encoding safely (does that add more than the max table size)
{-
1. If (size e) > 2^16, return e
2. Otherwise, return (add e s w)
-}
addEncoding :: Encoding -> String -> W.Word16 -> Encoding
addEncoding e s w
  | M.size e > maxSize = e
  | otherwise          = M.insert s w e
