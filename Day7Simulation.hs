module Day7Simulation where

import           Data.Bits           (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.HashMap.Strict (HashMap, empty, fromList, insert,
                                      insertWith, lookup, toList, (!))
import           Data.List           (foldl')
import           Data.Maybe          (fromMaybe)
import           Data.Word           (Word16)
import           GHC.Exts            (sortWith)
import           Prelude             hiding (lookup)


type Time        = Int
type WireName    = String
type Circuit a   = HashMap WireName (Wire a)

type Wire a      = (a, [Behaviour a])
type Behaviour a = Time -> Signal a
type Signal a    = (Time, Effect a)
type Effect a    = (WireName -> a) -> (WireName, a)

data System a    = System { circuit :: !(Circuit a)
                          , signals :: ![Signal a]
                          , time    :: !Time
                          }

type BinaryOp a
   = WireName    -- input wire 1
  -> WireName    -- input wire 2
  -> WireName    -- output wire
  -> Behaviour a -- resulting behaviour

type UnaryOp a
   = WireName    -- input wire
  -> WireName    -- output wire
  -> Behaviour a -- resulting behaviour


type Input       = (Circuit Word16, [Signal Word16])
type Solution    = Word16


main :: IO ()
main = readFile "inputs/day7.txt" >>= render . solve . parse


parse :: String -> Input
parse = foldl' push (empty, []) . lines
  where
    push (cir, sigs) inp =
      case init tokens of
        [              i, "->"] -> insertUnaryOp  1 id i
        [i,  "LSHIFT", n, "->"] -> insertUnaryOp  8 (`shiftL` read n) i
        [i,  "RSHIFT", n, "->"] -> insertUnaryOp  8 (`shiftR` read n) i
        [    "NOT",    i, "->"] -> insertUnaryOp  3 (`xor` maxBound) i
        [i1, "AND",   i2, "->"] -> insertBinaryOp 5 (.&.) i1 i2
        [i1, "OR",    i2, "->"] -> insertBinaryOp 6 (.|.) i1 i2

        _ -> error $ "Bad declaration:" ++ inp
      where
        tokens  = words inp
        outWire = last tokens

        insertUnaryOp timeout f i =
          case parseW16 i of
            Right v -> (cir, (timeout, const (outWire, f v)) : sigs)
            Left  _ -> (insertWire outWire []
                        $ insertWire i [unaryOp timeout f i outWire] cir,
                        sigs)

        insertBinaryOp timeout f i1 i2 =
          case (parseW16 i1, parseW16 i2) of
            (Left  _,  Left   _) ->
              let effs = [binaryOp timeout f i1 i2 outWire]
              in  (insertWire outWire []
                   $ insertWire i1 effs
                   $ insertWire i2 effs cir,
                   sigs)

            (Right v1, Right v2) ->
              (cir, (timeout, const (outWire, f v1 v2)) : sigs)

            (Right v1, _       ) -> insertUnaryOp timeout (v1 `f`) i2
            (_,        Right v2) -> insertUnaryOp timeout (`f` v2) i1

    insertWire wire effs =
      insertWith (\(v, effs1) (_, effs2) ->
                   let effs' = effs1 ++ effs2
                   in effs' `seq` (v, effs'))
      wire
      (0 :: Word16, effs)

    parseW16 :: String -> Either String Word16
    parseW16 s =
      case reads s of
        [(x, "")] -> Right x
        _         -> Left s

solve :: Input -> Solution
solve = fst . (! "a") . circuit . simulate . uncurry system

render :: Solution -> IO ()
render = print

--- behaviour constructing helpers & common operators

binaryOp
  :: Time          -- timeout
  -> (a -> a -> a) -- function to combine two input values
  -> BinaryOp a    -- resulting operation
binaryOp timeout comb i1 i2 out t =
  (t + timeout, \get -> (out, get i1 `comb` get i2))

unaryOp
  :: Time        -- timeout
  -> (a -> a)    -- value transforming function
  -> UnaryOp a   -- resulting operation
unaryOp timeout f  inp out t =
  (t + timeout, \get -> (out, f (get inp)))

directTo :: UnaryOp a
directTo = unaryOp 1 id

--- boolean logic

--- signal constructing helpers

touch :: WireName -> a -> Signal a
touch w v = (0, const (w, v))

--- simulation

system :: Circuit a -> [Signal a] -> System a
system c ss = System c (sortWith fst ss) 0

simulate :: System a -> System a
simulate =
  head . dropWhile (not . stabilized) . iterate step
  where
    stabilized = null . signals

step :: System a -> System a
step sys@(System _ [] _)  = sys
step sys@(System c ((sigTime, eff) : restSignals) currentTime)
  | sigTime > currentTime =
    sys { time = currentTime + 1 }
  | otherwise =
    let (wire, val)  = eff (fst . (c !?))
        (_, behs)    = c !? wire
        wireSignals  = map ($ currentTime) behs
        newSignals   = sortWith fst $ restSignals ++ wireSignals
    in  sys { circuit = insert wire (val, behs) c
            , signals = newSignals
            }

trace :: System a -> IO (System a)
trace = go 0
  where
    go tt s@(System _ ss t) =
      if null ss
        then return s
        else let s' = step s
             in  if tt /= t
                 then putStr (show t ++ ",") >> go t s'
                 else go tt s'

--- selftesting

example :: String
example =
  "123 -> x\n\
  \456 -> y\n\
  \x AND y -> d\n\
  \x OR y -> e\n\
  \x LSHIFT 2 -> f\n\
  \y RSHIFT 2 -> g\n\
  \NOT x -> h\n\
  \NOT y -> i"


worksFine :: Bool
worksFine = parseWorksFine && simulationWorksFine


parseWorksFine :: Bool
parseWorksFine =
  let (c, ss) = parse example
  in length ss == 2
     &&
     length (toList c) == 8
     &&
     contains c (zip
                 ["x", "y", "d", "e", "f", "g", "h", "i"]
                 $ replicate 8 0)


simulatesAnExampleProperly :: Bool
simulatesAnExampleProperly =
  let (c, ss) = parse example
      c' = circuit $ simulate $ system c ss
  in c' `contains` [("d", 72),
                    ("e", 507),
                    ("f", 492),
                    ("g", 114),
                    ("h", 65412),
                    ("i", 65079),
                    ("x", 123),
                    ("y", 456)]

simulationWorksFine :: Bool
simulationWorksFine =
  let andB :: BinaryOp Bool
      andB = binaryOp 5 (&&)

      notB :: UnaryOp Bool
      notB = unaryOp 2 not

      cir :: Circuit Bool
      cir = fromList [ ("a", (False, [ andB "a" "b" "c"
                                     , notB "a" "d"]))
                     , ("b", (False, [ andB "a" "b" "c"
                                     , notB "b" "e"]))
                     , ("c", (False, []))
                     , ("d", (True,  []))
                     , ("e", (True,  []))
                     ]
      cir' = circuit
             $ simulate
             $ system cir [ touch "a" True
                          , touch "b" True ]
  in
   cir  `contains` [("a", False),
                    ("b", False),
                    ("c", False),
                    ("d", True),
                    ("e", True)]
   &&
   cir' `contains` [("a", True),
                    ("b", True),
                    ("c", True),
                    ("d", False),
                    ("e", False)]


--- other helpers

contains :: Eq a => Circuit a -> [(WireName, a)] -> Bool
contains m ps =
  let (ws, vs) = unzip ps
  in and $ zipWith (==) vs $ map (fst . (m !)) ws

(!?) :: HashMap String a -> String -> a
(!?) m k = fromMaybe (error $ "Key error: " ++ k) $ lookup k m
