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
import qualified Data.DList as DL
import Data.Maybe

-- Stores the encodings of a string to bits
type Encoding = M.Map (DL.DList Char) W.Word16

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
compress s = BB.toLazyByteString $ foldr (\w b -> BB.word16BE w <> b)
              (BB.word8 $ fromIntegral (-1)) l 
  where t :: Encoding
        t = initTable
        l :: DL.DList W.Word16
        l = lzwCompress s t

-- Create the table with all possible single ascii characters
{-
1. Add all ascii to table with value [a..z,A..Z] key [0..255]
-}
initTable :: Encoding
initTable = foldr (\i e' -> M.insert (DL.singleton (C.chr i))
  (fromIntegral i) e') e [0..255]
    where e :: Encoding
          e = M.empty

-- Compress the string into a bit string using LZW
{-
1. Pattern match on string to find match in encoding table
2. Append Match to ByteString
3. Add new encoding (from nextPattern) with code of: (size encoding)
4. Recursively call append (result of getting match in map) lzwCompress
5. Base case: -1
-}
lzwCompress :: String -> Encoding -> DL.DList W.Word16
lzwCompress s@(x:xs) e = DL.append (DL.singleton code) (lzwCompress s' e')
    where (match, s') = nextPattern (DL.singleton x) xs e
          e' = case s' of
                []     -> e
                (x:xs) -> addEncoding e (DL.snoc match (head s')) (fromIntegral $ M.size e')
          code = fromMaybe (error ("Non-ASCII character " ++ DL.toList match ++ " encountered. Stopping.\n")) (M.lookup match e)
lzwCompress []    _ = DL.empty

-- Get next largest pattern that is in the LZW table
{-
1. s0 s1 e -> (match, s')
-}
nextPattern :: DL.DList Char -> String -> Encoding -> (DL.DList Char, String)
nextPattern s0 s1@(x:xs) e
  | M.member test e = nextPattern test xs e
  | otherwise       = (s0,s1)
    where test = DL.snoc s0 x
nextPattern s0 s1 e = (s0,s1)

-- Add to Encoding safely (does that add more than the max table size)
{-
1. If (size e) > 2^16, return e
2. Otherwise, return (add e s w)
-}
addEncoding :: Encoding -> DL.DList Char -> W.Word16 -> Encoding
addEncoding e s w
  | M.size e > maxSize = e
  | otherwise          = M.insert s w e
