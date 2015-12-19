module Day3 where

import Data.List (nub)
import Data.Maybe (mapMaybe)

data Direction = N | S | E | W

type Location  = (Int, Int)
type Grid      = [Location]
type Solution  = Int


start :: Location
start = (0, 0)


main :: IO ()
main = readFile "inputs/day3.txt" >>= render . solve2 . parse


parse :: String -> [Direction]
parse = mapMaybe toDirection
  where
    toDirection '^' = Just N
    toDirection 'v' = Just S
    toDirection '>' = Just E
    toDirection '<' = Just W
    toDirection  _  = Nothing


-- | Solver for the 1st subtask
solve :: [Direction] -> Solution
solve = numberOfUniques . locations


-- | Solver for the 2nd subtask
solve2 :: [Direction] -> Solution
solve2 = numberOfUniques . go [start] start start
  where
    go acc l1 l2  []    = l1 : l2 : acc
    go acc l1 l2 (d:ds) = go (l1 : acc) l2 (move d l1) ds


-- | Naive solver for the 2nd subtask (TODO: why it works wrong?)
solve2' :: [Direction] -> Solution
solve2' ds =
  let (ds1, ds2) = split ds
      ls1 = locations ds1
      ls2 = locations ds2
  in  numberOfUniques $ ls1 ++ ls2


locations :: [Direction] -> [Location]
locations = scanr move start


move :: Direction -> Location -> Location
move d (x, y) =
  case d of
    N -> (x, y + 1)
    S -> (x, y - 1)
    E -> (x + 1, y)
    W -> (x - 1, y)


split :: [a] -> ([a], [a])
split = foldr go ([], [])
  where
    go x (xs, ys) = (ys, x : xs)


render :: Solution -> IO ()
render = print


numberOfUniques :: Eq a => [a] -> Int
numberOfUniques = length . nub


worksFine :: Bool
worksFine =
     test solve   (2, 4, 2)
  && test solve2  (3, 3, 11)
  && test solve2' (3, 3, 11)
  where
    test solver (s1, s2, s3) =
      let s = solver . parse
      in  s "^v"         == s1
       && s "^>v<"       == s2
       && s "^v^v^v^v^v" == s3
