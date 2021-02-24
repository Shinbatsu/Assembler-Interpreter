module Main where

import System.IO
import qualified Interface as I

main :: IO ()
main = do
  putStrLn "Path to .asm file:"
  filename <- getLine
  fin  <- openFile filename ReadMode
  contents <- hGetContents fin
  I.runStrIO contents
  hClose fin
