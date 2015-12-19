module Day1 where

import Data.List (foldl')
import Data.Maybe (mapMaybe)

data Move  = Up | Down

type Pos   = Int
type Floor = Int

main :: IO ()
main = readFile "inputs/day1.txt" >>= render . solve . parse

parse :: String -> [Move]
parse = mapMaybe toMove
  where
    toMove '(' = Just Up
    toMove ')' = Just Down
    toMove _   = Nothing

solve :: [Move] -> (Pos, Floor)
solve = foldl' go (-1, 0) . zip [1..]
  where
    go (pos, floor') (cnt, move) =
      let nextFloor = floor' + case move of
            Up -> 1
            Down -> negate 1
      in  ( if pos >= 0
            then pos
            else
              if nextFloor < 0
              then cnt
              else -1
          , nextFloor
          )

render :: (Pos, Floor) -> IO ()
render (p, f) =
  putStrLn "Pos:" >> print p
  >> putStrLn "Floor:" >> print f
