{-
A command line tool for compressing files
-}

import qualified Compress
import qualified Decompress

main :: IO ()
{-
1. Read in file name and whether compressing or decompressing
2a. If compressing: read file f as string s, then Compress.compress s
2b. Write this bytestring to a new .hsc (Haskell Compression) file (*endianess?)
3a. If decompressing: read file f.hsc as bytestring bs, then
    Decompress.decompress s
3b. Write string to a new file called f without .hsc
-}
