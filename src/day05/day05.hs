module Main where

import Data.List
import Data.Ord
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
      ranges = map (normalize . parseRange) rangeLines
      ids = map read idLines
   in (ranges, ids)

isFresh :: [Range] -> Int -> Bool
isFresh ranges x =
  any (\(lo, hi) -> x >= lo && x <= hi) ranges

countLength :: Range -> Int
countLength (lo, hi) = hi - lo + 1

-- >>> countLength (8, 31)
-- 24
mergeRanges :: [Range] -> [Range]
mergeRanges rs = go (sortOn fst rs)
  where
    go [] = []
    go (r : rest) = step r rest

    step (lo, hi) [] = [(lo, hi)]
    step (lo, hi) ((lo2, hi2) : xs)
      | lo2 <= hi + 1 = step (lo, max hi hi2) xs
      | otherwise = (lo, hi) : step (lo2, hi2) xs

-- >>> mergeRanges [(1,3),(4,6),(0,3),(1,8)]
-- [(0,8)]

normalize :: Range -> Range -- probably uneccessary
normalize (a, b) = (min a b, max a b)

part1 :: Input -> Int
part1 lines =
  let (ranges, ids) = parseInput lines
   in length [i | i <- ids, isFresh ranges i]

part2 :: Input -> Int
part2 lines =
  let (ranges, _) = parseInput lines
   in sum $ map countLength $ mergeRanges ranges

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1: %d\n" $ part1 cont
  printf "Part 2: %d\n" $ part2 cont
