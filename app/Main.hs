module Main where

import Heart (pretty, heart)

main :: IO ()
main = do
  putStrLn $ pretty $ heart 5
  putStrLn $ pretty $ heart 6
