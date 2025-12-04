module Main where

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  print cont
  -- printf "Part 1: %d\n" $ part1 cont
  -- printf "Part 2: %d\n" $ part2 cont
