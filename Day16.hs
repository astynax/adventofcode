module Day16 where

import           Data.List (unfoldr)

type Amount   = Int
type Compound = String
type Property = (Compound, Amount)

type Aunt     = (Int, [Property])

type Test     = Compound -> Amount -> Amount -> Bool

type Input    = [Aunt]
type Solution = Int

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day16.txt"

  putStrLn "Part 1:"
  render $ solve exact input

  putStrLn "Part 2:"
  render $ solve notExact input


analysisResult :: [Property]
analysisResult =
  [ ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1)
  ]


parse :: String -> Input
parse = map parseLine . lines
  where
    parseLine s =
      case words s of
        ("Sue" : idx : ps) ->
          (read' idx, parseProps ps)
        _ -> badLine "bad prefix"
      where
        parseProps = unfoldr f
          where
            f x = case x of
              (c : a : xs) -> Just ((init c, read' a), xs)
              []           -> Nothing
              _            -> badLine "bad prop list"

        read' :: String -> Int
        read' x =
          case reads x of
            [(y, "")]  -> y
            [(y, ":")] -> y
            [(y, ",")] -> y
            _          -> badLine "bad number"

        badLine desc = error $ "Bad line: " ++ s ++ " (" ++ desc ++ ")"


solve :: Test -> Input -> Solution
solve test = fst . head . filter (satisfyTo test analysisResult . snd)

render :: Solution -> IO ()
render = print

--- helpers

satisfyTo :: Test -> [Property] -> [Property] -> Bool
satisfyTo test ps = not . any unfit
  where
    unfit (c, a) =
      case lookup c ps of
        (Just v) -> not $ test c a v
        _        -> error $ "Unknown compound: " ++ c

exact :: Test
exact _ x y = x == y

notExact :: Test
notExact compound =
  case compound of
    "cats"        -> (>)
    "trees"       -> (>)
    "goldfish"    -> (<)
    "pomeranians" -> (<)
    _             -> (==)

--- selfcheck

worksFine, parserWorks, calculationWorks :: Bool

worksFine =
  parserWorks

parserWorks =
  parse "Sue 42: children: 3, goldfish: 5" ==
  [ (42, [ ("children", 3)
         , ("goldfish", 5)
         ])
  ]

calculationWorks =
  solve exact aunts == 2
  &&
  solve notExact aunts == 3
  where
    aunts = [ (1, [ ("children", 3), ("akitas", 1) ])
            , (2, [ ("trees", 3), ("pomeranians", 3)])
            , (3, [ ("trees", 4), ("pomeranians", 2)]) ]
