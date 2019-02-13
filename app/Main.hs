module Main where

import           Lib (run)

main :: IO ()
main = interact $ unlines . map run . lines
