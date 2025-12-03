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

bestIndex :: Int -> String -> (Int, Char)
bestIndex _ [] = error ""
bestIndex allowedFirstsLen s@(c0 : _) = go 0 (0, c0)
  where
    go i best@(_, bestChar)
      | i >= allowedFirstsLen = best
      | otherwise =
          let c = s !! i
           in if c > bestChar
                then go (i + 1) (i, c)
                else go (i + 1) best

bestKthNumber :: Int -> String -> String
bestKthNumber k s
  | k <= 0 = "" -- for totalyti
  | length s <= k = s -- for totality otherwise == should be fine
  | otherwise =
      let allowedFirstsLen = length s - k + 1
          (bestIdx, bestChar) = bestIndex allowedFirstsLen s
       in bestChar : bestKthNumber (k - 1) (drop (bestIdx + 1) s)

maxJolt12 :: String -> Integer
maxJolt12 bank =
  let picked = bestKthNumber 12 bank
   in read picked

part2 :: Input -> Integer
part2 cont = sum $ map maxJolt12 cont

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  -- print $ map (map digitToInt) cont
  printf "Part 1: %d\n" $ part1 cont
  printf "Part 2: %d\n" $ part2 cont
