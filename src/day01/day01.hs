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
      let new = moveDial curr r
          acc' = new : acc -- will be reversed but shouldn't matter
       in go new rest acc'

part1 :: Input -> Int
part1 cont =
  let instructions = map parseRotation cont
      posList = positions 50 instructions
   in length (filter (== 0) posList)

hitsFrom :: Int -> Int -> Int
hitsFrom distanceToZero steps
  | distanceToZero > steps = 0
  | otherwise = 1 + (steps - distanceToZero) `div` 100

hitsDuring :: Int -> Rotation -> Int
hitsDuring curr (dir, steps) =
  case dir of
    'R' ->
      let base = (100 - curr) `mod` 100
          distanceToZero = if base == 0 then 100 else base
       in hitsFrom distanceToZero steps
    'L' ->
      let base = curr `mod` 100
          distanceToZero = if base == 0 then 100 else base
       in hitsFrom distanceToZero steps
    _ -> error ""

countAll :: Int -> [Rotation] -> Int -> Int
countAll _ [] total = total
countAll curr (r : rest) total =
  let hitsDuringRotation = hitsDuring curr r
      endPos = moveDial curr r
      newTotal = total + hitsDuringRotation
   in countAll endPos rest newTotal

part2 :: Input -> Int
part2 cont =
  let rotations = map parseRotation cont
   in countAll 50 rotations 0

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1: %d\n" $ part1 cont
  printf "Part 2: %d\n" $ part2 cont
