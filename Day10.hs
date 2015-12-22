module Day10 where

import           Control.Arrow ((&&&))
import           Data.List     (group)

type Input    = String
type Solution = Int

main :: IO ()
main = do
  let input = "1321131112"

  putStrLn "Part 1:"
  render $ solveFor 40 input

  putStrLn "Part 2:"
  render $ solveFor 50 input


solveFor :: Int -> Input -> Solution
solveFor n = length . head . drop n . iterate grow

render :: Solution -> IO ()
render = print


grow :: String -> String
grow = concatMap (uncurry (++) . (show . length &&& (:[]) . head)) . group

--- selftesting

worksFine :: Bool
worksFine =
  grow "1321131112" == "11131221133112"
