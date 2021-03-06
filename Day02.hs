module Day02 where

import           Control.Arrow ((&&&), (***))
import           Data.List (sort)
import qualified Data.Text as T

type Feet       = Int
type Area       = Int
type Length     = Int
type Dimensions = (Feet, Feet, Feet)
type Solution   = (Area, Length)

main :: IO ()
main = readFile "inputs/day2.txt" >>= render . solve . parse

parse :: String -> [Dimensions]
parse = map parseOne . lines
  where
    parseOne s =
      let [l, w, h] = sort . map (read . T.unpack)
                      . T.split (== 'x') . T.pack $ s
      in  (l, w, h)

solve :: [Dimensions] -> Solution
solve = (sum *** sum) . unzip . map (area &&& ribbon)

area :: Dimensions -> Feet
area (l, w, h) = 3 * l * w +
                 2 * l * h +
                 2 * w * h

ribbon :: Dimensions -> Feet
ribbon (l, w, h) = l * w * h +
                   l + l + w + w

render :: Solution -> IO ()
render (a, l) = do
  putStrLn "Total paper:"
  print a

  putStrLn "Total ribbon:"
  print l
