module Day09 where

import           Data.List       (foldl', intercalate, nub)
import qualified Data.Map        as M
import           Data.Map.Strict (Map, empty, insertWith, toList)
import           Data.Maybe      (fromMaybe)
import           GHC.Exts        (sortWith)

type Location = String
type Distance = Int
type SubRoute = (Distance, Location)
type Route    = (Distance, [Location])
type RoadMap  = Map Location [SubRoute]

type Input    = RoadMap
type Solution = Route


main :: IO ()
main = do
  rs <- allRoutes <$> getData

  putStrLn "Part 1:"
  render . solve $ rs

  putStrLn "Part 2:"
  render . solve2 $ rs


getData :: IO Input
getData = parse <$> readFile "inputs/day9.txt"


parse :: String -> Input
parse = foldl' insert empty . lines
  where
    insert m l = let (f, t, d) = parseLine l
                 in insertWith   (++) f [(d, t)]
                    $ insertWith (++) t [(d, f)]
                    m
    parseLine x =
      case words x of
        [from, "to", to, "=", dist] -> (from, to, read dist)
        _ -> error $ "Bad subroute: " ++ x


allRoutes :: RoadMap -> [Route]
allRoutes m = sortWith fst $ concatMap (routes m) (starts m)

solve :: [Route] -> Solution
solve = head

solve2 :: [Route] -> Solution
solve2 = last


render :: Solution -> IO ()
render (d, ls) = putStrLn $ intercalate " -> " (reverse ls) ++ " = " ++ show d

--- helpers

routes :: RoadMap -> Location -> [Route]
routes rm loc = routes' 0 [loc] rm loc
  where
    routes'
      :: Distance
      -> [Location]
      -> RoadMap
      -> Location
      -> [Route]
    routes' dist visited roadmap location =
      let
        subroutes =
          filter (not . (`elem` visited) . snd)
          $ roadmap !? location
      in if null subroutes
         then [(dist, visited)]
         else do
           (d, l) <- subroutes
           routes' (dist + d) (l : visited) roadmap l


starts :: Input -> [Location]
starts = nub . map fst . toList

(!?) :: Map String a -> String -> a
(!?) m k = fromMaybe (error $ "Key error: " ++ k) $ M.lookup k m

--- selfcheck

example :: String
example =
  "London to Dublin = 464\n\
  \London to Belfast = 518\n\
  \Dublin to Belfast = 141"

worksFine :: Bool
worksFine =
  resultOfUsing solve == 605
  &&
  resultOfUsing solve2 == 982
  where
    resultOfUsing f = fst . f . allRoutes . parse $ example
