module Main where

import Data.Char
import Text.Printf

type Input = [String]

maxJolt :: String -> Int
maxJolt bank =
  maximum
    [ 10 * d1 + d2
    | (d1, i) <- digitsWithIndex,
      (d2, j) <- digitsWithIndex,
      i < j
    ]
  where
    digitsWithIndex = zip (map digitToInt bank) [0 ..]

part1 :: Input -> Int
part1 cont = sum $ map maxJolt cont

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  -- print $ map (map digitToInt) cont
  printf "Part 1: %d\n" $ part1 cont
