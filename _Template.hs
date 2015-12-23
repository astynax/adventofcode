module _ where

type Input    = _
type Solution = _

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day_.txt"_

  putStrLn "Part 1:"
  render $ solve input

  putStrLn "Part 2:"
  render $ solve2 input

parse :: String -> Input
parse = undefined

solve :: Input -> Solution
solve = undefined

solve2 :: Input -> Solution
solve2 = undefined

render :: Solution -> IO ()
render = undefined
