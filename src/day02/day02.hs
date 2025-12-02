module Main where

import Data.List.Split
import Text.Printf

type Input = [String]

type Range = (Int, Int)

parseInput :: String -> Range
parseInput s =
  let (a, _ : b) = break (== '-') s
   in (read a, read b)

listGenerator :: Range -> [Int]
listGenerator (a, b) = [a .. b]

isInvalid :: Int -> Bool
isInvalid n =
  let s = show n
      len = length s
   in even len
        && let (a, b) = splitAt (len `div` 2) s
            in a == b

part1 :: Input -> Int
part1 cont =
  let ranges = map parseInput cont
      allIds = concatMap listGenerator ranges
      invalidIds = filter isInvalid allIds
   in sum invalidIds

main :: IO ()
main = do
  raw <- readFile "input.txt"
  let cont = splitOn "," raw
  printf "Part 1: %d\n" $ part1 cont
  -- printf "Part 2: %d\n" $ part2 cont

-- print $ map listGenerator $ map parseInput cont
-- print $ map (listGenerator . parseInput) cont
