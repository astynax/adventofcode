module Day13 where

import           Data.List (foldl', permutations)
import           Data.Map  (Map, empty, fromList, insert, insertWith, keys,
                            singleton, union, (!))

type Person      = String
type Happiness   = Int
type Arrangement = [Person]

type Input       = Map Person (Map Person Happiness)
type Solution    = Happiness

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day13.txt"

  putStrLn "Part 1:"
  render $ solve input

  putStrLn "Part 2:"
  render $ solve2 input


parse :: String -> Input
parse = foldl' push empty . map parseLine . lines
  where
    parseLine l = case words (init l) of
      [p1, "would", verb, hp, _, _, _, _, _, _, p2] ->
        let hp' = read hp * case verb of
              "gain" -> 1
              "lose" -> -1
              _      -> error $ "Bad verb: " ++ verb
        in  (p1, p2, hp')
      _ -> error $ "Bad line: " ++ l

    push m (p1, p2, hp) =
      insertWith union p1 (singleton p2 hp) m


solve :: Input -> Solution
solve m = maximum . map (calculate (\p1 p2 -> m ! p1 ! p2)) . arrangements $ m

solve2 :: Input -> Solution
solve2 = solve . withMe

render :: Solution -> IO ()
render = print


arrangements :: Input -> [Arrangement]
arrangements m =
  let (p:ps) = keys m
  in map (p :) $ permutations ps

calculate :: (Person -> Person -> Happiness) -> Arrangement -> Happiness
calculate feelsTo ps =
  let ps' = ps ++ [head ps]
  in  sum $ zipWith happiness ps' (tail ps')
  where
    happiness p1 p2 = p1 `feelsTo` p2 + p2 `feelsTo` p1

withMe :: Input -> Input
withMe m =
  let others = keys m
  in insert "Me" (fromList $ zip others (repeat 0))
     $ foldl' (flip $ \p -> insertWith union p (singleton "Me" 0))
     m others

--- selfcheck

example :: String
example =
  "Alice would gain 54 happiness units by sitting next to Bob.\n\
  \Alice would lose 79 happiness units by sitting next to Carol.\n\
  \Alice would lose 2 happiness units by sitting next to David.\n\
  \Bob would gain 83 happiness units by sitting next to Alice.\n\
  \Bob would lose 7 happiness units by sitting next to Carol.\n\
  \Bob would lose 63 happiness units by sitting next to David.\n\
  \Carol would lose 62 happiness units by sitting next to Alice.\n\
  \Carol would gain 60 happiness units by sitting next to Bob.\n\
  \Carol would gain 55 happiness units by sitting next to David.\n\
  \David would gain 46 happiness units by sitting next to Alice.\n\
  \David would lose 7 happiness units by sitting next to Bob.\n\
  \David would gain 41 happiness units by sitting next to Carol."

worksFine, parsingWorks, calculationWorks, exampleWorks :: Bool

worksFine =
  parsingWorks
  &&
  calculationWorks
  &&
  exampleWorks

parsingWorks =
  ex ! "Alice" ! "Bob"   == 54
  &&
  ex ! "Alice" ! "Carol" == -79
  &&
  ex ! "Bob"   ! "Alice" == 83
  &&
  ex ! "David" ! "Bob"   == -7
  where
    ex = parse example

calculationWorks =
  calculate f ["a", "b", "c"] == (5+10 + (-1)+3 + 8+8)
  where
    f "a" "b" = 5
    f "a" "c" = 10
    f "b" "a" = -1
    f "b" "c" = 3
    f _   _   = 8

exampleWorks =
  solve (parse example) == 330
