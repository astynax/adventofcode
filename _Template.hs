module _ where

type Input    = _
type Solution = _

main :: IO ()
main = readFile "inputs/day_.txt"_ >>= render . solve . parse

parse :: String -> [Input]
parse = undefined

solve :: [Input] -> Solution
solve = undefined

render :: Solution -> IO ()
render = undefined
