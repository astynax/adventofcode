-- stack --install-ghc runghc --package aeson --package bytestring
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import           Data.Aeson           (Object, Value (..), decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe           (fromMaybe)
import           Data.Scientific      (toRealFloat)
import qualified Data.Vector          as V

type Input    = Value
type Solution = Float

main :: IO ()
main = do
  input <- parse <$> BSL.readFile "inputs/day12.txt"

  putStrLn "Part 1:"
  render $ solve input

  putStrLn "Part 2:"
  render $ solve2 input

parse :: BSL.ByteString -> Input
parse = fromMaybe (error "Bad JSON!") . decode

solve :: Input -> Solution
solve = sum' (const True)

solve2 :: Input -> Solution
solve2 = sum' isntRed

render :: Solution -> IO ()
render = print


sum' :: (Object -> Bool) -> Value -> Float
sum' accept = \case
  Number x            -> toRealFloat x
  Array  v            -> V.sum . V.map (sum' accept) $ v
  Object o | accept o -> sum . map (sum' accept) . HM.elems $ o
  _                   -> 0

isntRed :: Object -> Bool
isntRed = not . (String "red" `elem`) . HM.elems

--- selfcheck

worksFine :: Bool
worksFine =
  solve2 (parse "[1,2,3]") == 6
  &&
  solve2 (parse "[1,{\"c\":\"red\",\"b\":2},3]") == 4
  &&
  solve2 (parse "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") == 0
  &&
  solve2 (parse "[1,\"red\",5]") == 6
