module _ where

type Input    = _
type Solution = _

main :: IO ()
main =
  putStrLn "Part 1:" >> run solve
  >>
  putStrLn "Part 2:" >> run solve2
  where
    run f = readFile "inputs/day_.txt"_ >>= render . f . parse

parse :: String -> [Input]
parse = undefined

solve :: [Input] -> Solution
solve = undefined

solve2 :: [Input] -> Solution
solve2 = undefined

render :: Solution -> IO ()
render = undefined
