module Day06_2 where

import           Data.List       (foldl')
import           Data.Map.Strict (Map, empty, fromList, mergeWithKey, toList)

import           Day06            hiding (Grid, main, solve, update)


type Grid = Map Coords Int


main :: IO ()
main = readFile "inputs/day6.txt" >>= render . solve . parse


solve :: [Input] -> Solution
solve = sum . map snd . toList . foldl' f empty
  where
    f g (act, c1, c2) = update g act (area c1 c2)


-- helpers

update :: Grid -> Action -> [Coords] -> Grid
update g act cs =
  let (merge, apply, patchValue) = case act of
        Toggle   ->
          (\_ old _ -> Just (old + 2),         keepNew,   2)
        Turn On  ->
          (\_ old _ -> Just (old + 1),         keepNew,   1)
        Turn Off ->
          (\_ old _ -> Just (max (old - 1) 0), rejectNew, 0)
  in  mergeWithKey merge id apply g (mkPatch patchValue)
  where
    keepNew   = id
    rejectNew = const empty

    mkPatch   = fromList . zip cs . repeat

-- selfcheck

worksFine :: Bool
worksFine =
  let g1 = update empty (Turn On)  [(0, 0), (1, 1)]
      g2 = update g1    Toggle     [(1, 1), (2, 2), (3, 3)]
      g3 = update g2    (Turn Off) [(0, 0), (2, 2)]
      g4 = update g3    (Turn Off) [(0, 0)]
  in  g4 == fromList [ ((0, 0), 0)
                     , ((1, 1), 3)
                     , ((2, 2), 1)
                     , ((3, 3), 2)
                     ]
