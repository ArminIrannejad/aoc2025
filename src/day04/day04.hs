module Main where

import Text.Printf

type Input = [String]

type Position = (Int, Int)

type Direction = (Int, Int)

directions :: [Direction]
directions =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

height :: Input -> Int
height = length

width :: Input -> Int
width [] = 0
width (r : _) = length r

inBounds :: Input -> Position -> Bool
inBounds ls (r, c) =
  r >= 0
    && r < height ls
    && c >= 0
    && c < width ls

lookupPos :: Input -> Position -> Maybe Char
lookupPos ls p@(r, c)
  | not (inBounds ls p) = Nothing
  | otherwise = Just ((ls !! r) !! c)

neighbors :: Input -> Position -> [Position]
neighbors ls (r, c) =
  [ (r + dr, c + dc)
  | (dr, dc) <- directions,
    let p = (r + dr, c + dc),
    inBounds ls p
  ]

isAccessible :: Input -> Position -> Bool
isAccessible ls p =
  case lookupPos ls p of
    Just '@' ->
      let n =
            length
              [ ()
              | np <- neighbors ls p,
                lookupPos ls np == Just '@'
              ]
       in n < 4
    _ -> False

part1 :: Input -> Int
part1 ls =
  length
    [ ()
    | r <- [0 .. height ls - 1],
      c <- [0 .. width ls - 1],
      isAccessible ls (r, c)
    ]

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1 %d\n" $ part1 cont
