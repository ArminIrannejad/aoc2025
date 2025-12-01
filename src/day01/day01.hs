module Main where

import Text.Printf

type Input = [String]

type Rotation = (Char, Int)

parseRotation :: String -> Rotation
parseRotation (c : ns) = (c, read ns) -- guess partial
parseRotation _ = error ""

moveDial :: Int -> Rotation -> Int
moveDial curr (dir, n) =
  case dir of
    'L' -> (curr - n) `mod` 100
    'R' -> (curr + n) `mod` 100
    _ -> error ""

positions :: Int -> [Rotation] -> [Int]
positions start rs = go start rs []
  where
    go _ [] acc = acc
    go curr (r : rest) acc =
      -- will be reverse order but shouldn't matter
      let new = moveDial curr r
          acc' = new : acc
       in go new rest acc'

part1 :: Input -> Int
part1 cont =
  let instructions = map parseRotation cont
      posList = positions 50 instructions
   in length (filter (== 0) posList)

part2 :: Input -> Int
part2 _ = error "TODO"

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1: %d\n" $ part1 cont

-- printf "Part 2: %d\n" $ part2 cont
