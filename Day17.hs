module Day17 where

import GHC.Exts (sortWith)
import Data.List (permutations, inits, nub, sort)

type Volume   = Int

type Input    = [Volume]
type Solution = [[Volume]]

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day17.txt"

  putStrLn "Part 1:"
  render $ solveTask1For 150 input

  putStrLn "Part 2:"
  render $ solveTask2For 150 input

parse :: String -> Input
parse = map read . lines

solveTask1For :: Int -> Input -> Solution
solveTask1For = combinations'

solveTask2For :: Int -> Input -> Solution
solveTask2For = ((limit . sortWith length) .) . combinations'
  where
    limit xs@(x : _) = takeWhile ((== length x) . length) xs
    limit _          = []

render :: Solution -> IO ()
render = print . length

--- helpers

-- naive version
combinations :: Volume -> [Volume] -> [[Volume]]
combinations v =
  filter ((v ==) . sum)
  . map (map snd)
  . nub . map (sortWith fst)
  . concatMap inits
  . permutations
  . zip [0 :: Int ..]

combinations' :: Volume -> [Volume] -> [[Volume]]
combinations' 0 _         = [[]]
combinations' v _ | v < 0 = []
combinations' _ []        = []
combinations' v [x]       = [[x] | x == v]
combinations' v (x:xs)    =
  map (x :) (combinations' (v - x) xs)
  ++
  combinations' v xs

--- selfcheck

tryExample :: (Int -> Input -> Solution) -> Solution
tryExample s = s 25 [20, 15, 10, 5, 5]

worksFine, combiningWorks, exampleWorks :: Bool

worksFine =
  combiningWorks
  &&
  exampleWorks

combiningWorks =
  worksProperly combinations
  &&
  worksProperly combinations'
  where
    worksProperly = (== solution) . sort . tryExample

    solution =
      [ [15, 5, 5]
      , [15, 10]
      , [20, 5]
      , [20, 5]
      ]

exampleWorks =
  run solveTask1For == 4
  &&
  run solveTask2For == 3
  where
    run = length . tryExample
