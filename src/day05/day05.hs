module Main where

import Text.Printf

type Input = [String]
type Range = (Int, Int)

splitOnBlank :: [String] -> ([String], [String])
splitOnBlank [] = ([], [])
splitOnBlank ("" : rest) = ([], rest)
splitOnBlank (x : xs) =
  let (as, bs) = splitOnBlank xs
   in (x : as, bs)

parseRange :: String -> Range
parseRange s =
  let (a, _ : b) = break (== '-') s
   in (read a, read b)

parseInput :: Input -> ([Range], [Int])
parseInput lines =
  let (rangeLines, idLines) = splitOnBlank lines
      ranges = map parseRange rangeLines
      ids    = map read idLines
   in (ranges, ids)

isFresh :: [Range] -> Int -> Bool
isFresh ranges x =
  any (\(lo, hi) -> x >= lo && x <= hi) ranges

part1 :: Input -> Int
part1 lines =
  let (ranges, ids) = parseInput lines
   in length [ i | i <- ids, isFresh ranges i ]

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1: %d\n" (part1 cont)

