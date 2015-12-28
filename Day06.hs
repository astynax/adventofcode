module Day06 where

import           Data.List       (foldl')
import           Data.Map.Strict (Map, empty, fromList, mergeWithKey, size)
import           Data.Maybe      (fromMaybe)

data State    = On
              | Off deriving Eq

data Action   = Turn State
              | Toggle deriving Eq

type Coords   = (Int, Int)
type Grid     = Map Coords ()

type Input    = (Action, Coords, Coords)
type Solution = Int


main :: IO ()
main = readFile "inputs/day6.txt" >>= render . solve . parse


parse :: String -> [Input]
parse = map parseOne . lines
  where
    parseOne i =
      let (c1, c2, act) = case words i of
            ["toggle",      f, "through", t] -> (f, t, Toggle)
            ["turn", "on",  f, "through", t] -> (f, t, Turn On)
            ["turn", "off", f, "through", t] -> (f, t, Turn Off)
            _ -> error $ "Bad input: " ++ i
      in  (act, parseCoords c1, parseCoords c2)

    parseCoords :: String -> Coords
    parseCoords s =
      fromMaybe (error $ "Bad coord:" ++ s)
      $ case reads s of
        [(x, ',' : s')] | inRange x ->
          case reads s' of
            [(y, "")] | inRange y ->
              Just (x, y)
            _ -> Nothing
        _ -> Nothing

    inRange x = x >= 0 && x < 1000


solve :: [Input] -> Solution
solve = size . foldl' f empty
  where
    f g (act, c1, c2) = update g act (area c1 c2)


render :: Solution -> IO ()
render = print

-- helpers

update :: Grid -> Action -> [Coords] -> Grid
update g act cs =
  let (merge, apply) = case act of
        Turn On  -> ( keepOld,   keepNew )
        Turn Off -> ( rejectOld, rejectNew )
        Toggle   -> ( rejectOld, keepNew )
  in  mergeWithKey merge id apply g patch
  where
    keepOld   _ _ _ = Just ()
    rejectOld _ _ _ = Nothing
    keepNew         = id
    rejectNew       = const empty

    patch = fromList $ zip cs $ repeat ()

area :: Coords -> Coords -> [Coords]
area (x1, y1) (x2, y2) =
  [ (x, y)
  | y <- [min y1 y2 .. max y1 y2]
  , x <- [min x1 x2 .. max x1 x2]
  ]

-- selfcheck

worksFine :: Bool
worksFine = parseWorksFine && updateWorksFine


parseWorksFine :: Bool
parseWorksFine =
  parse ("turn off 0,0 through 1,1\n" ++
         "toggle 2,4 through 4,2\n" ++
         "turn on 100,100 through 900,900"
        ) == [ (Turn Off, (0,   0),   (1,   1))
             , (Toggle,   (2,   4),   (4,   2))
             , (Turn On,  (100, 100), (900, 900)) ]


updateWorksFine :: Bool
updateWorksFine =
  update    src Toggle     [(0, 0), (1, 0)] == grid [(1, 0), (1, 1)]
  && update src (Turn Off) [(0, 0), (1, 0)] == grid [(1, 1)]
  && update src (Turn On)  [(0, 0), (1, 0)] == grid [(0, 0), (1, 0), (1, 1)]
  where
    src = grid [(0, 0), (1, 1)]
    grid = fromList . flip zip (repeat ())
