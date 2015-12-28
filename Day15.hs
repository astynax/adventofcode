module Day15 where

import Data.List (foldl')

type Amount = Int

data Ingredient =
  Ing { name       :: String
      , capacity   :: Int
      , durability :: Int
      , flavor     :: Int
      , texture    :: Int
      , calories   :: Int
      }
  deriving (Eq, Show)

type Input    = [Ingredient]
type Solution = Int

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day15.txt"

  putStrLn "Part 1:"
  render $ solve input

  putStrLn "Part 2:"
  render $ solve2 input

parse :: String -> Input
parse = map parseLine . lines
  where
    parseLine s =
      case words s of
        [ name'
          , "capacity",   cap
          , "durability", dur
          , "flavor",     flav
          , "texture",    tex
          , "calories",   cal ]
          -> Ing { name       = init  name'
                 , capacity   = read' cap
                 , durability = read' dur
                 , flavor     = read' flav
                 , texture    = read' tex
                 , calories   = read  cal
                 }
        _ -> error $ "Bad line: " ++ s

    read' = read . init

solve :: Input -> Solution
solve ings =
  maximum
  $ map (cookieScore . zip ings)
  $ portions 100 (length ings)

solve2 :: Input -> Solution
solve2 ings =
  maximum
  $ map cookieScore
  $ filter (hasNeedAmountOfCalories 500)
  $ map (zip ings)
  $ portions 100 (length ings)

render :: Solution -> IO ()
render = print

--- helpers

cookieScore :: [(Ingredient, Amount)] -> Int
cookieScore =
  product . map (max 0)
  . foldl' (zipWith (+)) [0, 0, 0, 0]
  . map (uncurry ingScore)
  where
    ingScore (Ing _ c d f t _) n = [n * c, n * d, n * f, n * t]

portions
  :: Amount     -- total amount
  -> Int        -- ingredient count
  -> [[Amount]] -- ingredient amounts
portions m 1 = [[m]]
portions m n = do
  x <- [0..m]
  xs <- portions (m - x) (n - 1)
  return $ x : xs

hasNeedAmountOfCalories :: Int -> [(Ingredient, Amount)] -> Bool
hasNeedAmountOfCalories x = (x ==) . sum . map (\(i, n) -> calories i * n)

--- selfcheck

example :: String
example =
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\n\
  \Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"

worksFine, parseWorks, calculationWorks, portioningWorks :: Bool

worksFine =
  parseWorks
  &&
  calculationWorks
  &&
  portioningWorks

parseWorks =
  parse example ==
  [ Ing "Butterscotch" (-1) (-2) 6    3    8
  , Ing "Cinnamon"     2    3    (-2) (-1) 3
  ]

calculationWorks =
  let ings        = parse example
      recipie     = zip ings [44, 56]
      targetScore = 62842880
  in  cookieScore recipie == targetScore
      &&
      solve ings == targetScore
      &&
      solve2 ings == 57600000

portioningWorks =
  portions 2 3 ==
    [ [0, 0, 2]
    , [0, 1, 1]
    , [0, 2, 0]
    , [1, 0, 1]
    , [1, 1, 0]
    , [2, 0, 0]
    ]
