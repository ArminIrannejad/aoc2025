module Main where

import Text.Printf
import Data.List.Split

type Input = [String]

parseInput :: String -> (Int, Int)
parseInput s =
  let (a, _ : b) = break (== '-') s
   in (read a, read b)

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let cont = splitOn "," raw
  print $ map parseInput cont

-- printf "Part 1: %d\n" $ part1 cont
-- printf "Part 2: %d\n" $ part2 cont
