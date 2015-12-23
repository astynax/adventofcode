module Day14 where

import           Data.Char (isDigit)
import           Data.List (foldl')

type Name     = String
type Speed    = Int
type Seconds  = Int
type RunTime  = Seconds
type Timeout  = Seconds
type Distance = Int
type Score    = Int

type Deer     = (Name, Speed, RunTime, Timeout)

type Input    = [Deer]
type Solution = Int

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day14.txt"

  putStrLn "Part 1:"
  render $ solve input

  putStrLn "Part 2:"
  render $ solve2 input

parse :: String -> Input
parse = map parseLine . lines
  where
    parseLine l =
      let (name:rest) = words l
          [s,rt,to] = map read $ filter (isDigit . head) rest
      in  (name, s, rt, to)

solve :: Input -> Int
solve = maximum . map (fliedAfter 2503)

solve2 :: Input -> Int
solve2 = maximum . scoresAfter 2503

render :: Int -> IO ()
render = print

--- helpers

fliedAfter :: Seconds -> Deer -> Distance
fliedAfter time (_, speed, rt, tout) =
  let cycle'         = rt + tout
      cycleDist      = rt * speed
      (cycles, rem') = divMod time cycle'
  in  cycles * cycleDist + (min rem' rt) * speed

scoresAfter :: Seconds -> [Deer] -> [Score]
scoresAfter time ds =
  let kms    = transpose . map kilometers $ ds
      scores = replicate (length ds) 0
  in  foldl' givePoints scores kms
  where
    kilometers = take time . scanl1 (+) . steps

    steps (_, speed, rt, tout) =
      concat . cycle $ [replicate rt speed, replicate tout 0]

    givePoints ss kms =
      let maxKm = maximum kms
      in  zipWith (\s km -> if km == maxKm then s + 1 else s)
          ss kms

transpose :: [[a]] -> [[a]]
transpose =
  map (map head)
  . takeWhile (not . null . head)
  . iterate (map tail)


--- selfcheck

example :: String
example =
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
  \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

worksFine, parsingWorks, calculationWorks :: Bool

worksFine =
  parsingWorks
  &&
  calculationWorks

parsingWorks =
  parse example ==
  [ ("Comet",  14, 10, 127)
  , ("Dancer", 16, 11, 162) ]

calculationWorks =
  let [comet, dancer] = parse example
  in  fliedAfter 1000 comet  == 1120
      &&
      fliedAfter 1000 dancer == 1056
      &&
      scoresAfter 1000 [comet, dancer] == [312, 689]
