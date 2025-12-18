module Main where

import Data.Char
import Text.Printf

type Input = [String]

type Answer = Int

solveProblemPart1 :: [String] -> Answer
solveProblemPart1 segLines =
  let toks =
        [ t
        | line <- segLines,
          let t = trim line,
          not (null t)
        ]
      opTok = last toks
      op =
        case opTok of
          [c] -> c
          _ -> error ("TF is this!?!?!?" ++ opTok)
      nums = map (read :: String -> Answer) (init toks)
   in applyAll (opFun op) nums

solveProblemPart2 :: [String] -> Answer
solveProblemPart2 segLines =
  let opRow = last segLines
      op = firstNonSpace opRow

      digitRows = init segLines
      w = length opRow

      nums =
        [ readDigitsTopToBottom digitRows j
        | j <- [w - 1, w - 2 .. 0],
          colHasDigit digitRows j
        ]
   in applyAll (opFun op) nums

colHasDigit :: [String] -> Int -> Bool
colHasDigit rows j = any (\row -> isDigit (row !! j)) rows

readDigitsTopToBottom :: [String] -> Int -> Answer
readDigitsTopToBottom rows j =
  let ds = [row !! j | row <- rows, isDigit (row !! j)]
   in read ds

firstNonSpace :: String -> Char
firstNonSpace s =
  case dropWhile isSpace s of
    (c : _) -> c
    [] -> error ""

opFun :: Char -> (Answer -> Answer -> Answer)
opFun op =
  case op of
    '+' -> (+)
    '*' -> (*)
    _ -> error [op]

applyAll :: (Answer -> Answer -> Answer) -> [Answer] -> Answer
applyAll _ [] = error ""
applyAll _ [x] = x
applyAll f (x : xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) = go (f acc y) ys

rangesTrue :: [Bool] -> [(Int, Int)]
rangesTrue = go 0
  where
    go _ [] = []
    go i xs =
      case dropWhile not xs of
        [] -> []
        ys ->
          let start = i + (length xs - length ys)
              (block, rest) = span id ys
              end = start + length block - 1
           in (start, end) : go (end + 1) rest

slice :: Int -> Int -> String -> String
slice l r s = take (r - l + 1) (drop l s)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

grandTotalWith :: ([String] -> Answer) -> Input -> Answer
grandTotalWith solveOne ls0 =
  let w = maximum (map length ls0)
      ls = map (padRight w) ls0

      sepCols = [all (\row -> row !! i == ' ') ls | i <- [0 .. w - 1]]
      goodCols = map not sepCols

      problemRanges = rangesTrue goodCols

      problems =
        [ [slice l r row | row <- ls]
        | (l, r) <- problemRanges
        ]
   in sum (map solveOne problems)

part1 :: Input -> Answer
part1 = grandTotalWith solveProblemPart1

part2 :: Input -> Answer
part2 = grandTotalWith solveProblemPart2

main :: IO ()
main = do
  cont <- lines <$> readFile "input.txt"
  printf "Part 1: %d\n" $ part1 cont
  printf "Part 2: %d\n" $ part2 cont
