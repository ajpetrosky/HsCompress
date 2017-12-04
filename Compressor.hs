{-
A command line tool for compressing files
-}

import qualified Compress
import qualified Decompress
import qualified Data.ByteString.Lazy as B
import Data.Text as T
import System.Environment
import System.Exit

{-
1. Read in file name and whether compressing or decompressing
2a. If compressing: read file f as string s, then Compress.compress s
2b. Write this bytestring to a new .hsc (Haskell Compression) file (*endianess?)
3a. If decompressing: read file f.hsc as bytestring bs, then
    Decompress.decompress s
3b. Write string to a new file called f without .hsc
-}
main :: IO()
main = getArgs >>= parse

parse :: [String] -> IO()
parse ("-c":[fs]) = c fs >> exitSuccess
parse ("-d":[fs]) = d fs >> exitSuccess
parse _ = putStrLn "Needs a flag -c or -d for compression or decompression respectively, followed by a file name." >> failure

failure :: IO()
failure  = exitWith (ExitFailure 1)

-- Compress the given file
c :: String -> IO()
c fp = do
    file <- Prelude.readFile fp
    B.writeFile (fp ++ ".hsc") (Compress.compress file)

-- Decompress the given file
d :: String -> IO()
d fp = do
    file <- B.readFile fp
    let name = removeExt fp
    Prelude.writeFile name (Decompress.decompress file)

removeExt :: String -> String
removeExt ['.','h','s','c'] = []
removeExt (x:xs)            = x : removeExt xs
removeExt _                 = error "Invalid file extension."
