module Day07 where

import           Data.Bits       (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.List       (foldl')
import           Data.Map.Strict (Map, empty, fromList, insert, lookup, toList)
import           Data.Maybe      (fromMaybe)
import           Data.Word       (Word16)
import           Prelude         hiding (lookup)

type Wire     = String

data Value    = Const { unConst :: Word16 }
              | Cont Wire (Word16 -> Value)

type Input    = Map Wire Value
type Solution = Word16


main :: IO ()
main =
  putStrLn "Part 1:" >> run solve
  >>
  putStrLn "Part 2:" >> run solve2
  where
    run f = readFile "inputs/day7.txt" >>= render . f . parse


parse :: String -> Input
parse = foldl' push empty . lines
  where
    push :: Input -> String -> Input
    push cir inp =
      let tokens = words inp
          wire   = last tokens
      in (\v -> insert wire v cir) $
         case init tokens of
           [              v, "->"] -> unaryOp  id v
           [i,  "LSHIFT", n, "->"] -> unaryOp  (`shiftL` read n) i
           [i,  "RSHIFT", n, "->"] -> unaryOp  (`shiftR` read n) i
           [    "NOT",    i, "->"] -> unaryOp  (`xor` maxBound)    i
           [i1, "AND",   i2, "->"] -> binaryOp (.&.) i1 i2
           [i1, "OR",    i2, "->"] -> binaryOp (.|.) i1 i2

           _ -> error $ "Bad declaration:" ++ inp

    unaryOp  f w =
      case parseW16 w of
        Left _  -> Cont w $ Const . f
        Right v -> Const v

    binaryOp f w1 w2 =
      case (parseW16 w1, parseW16 w2) of
        (Left  _, Left  _) -> Cont w1 $ Cont w2 . (Const .) . f
        (Right x, Right y) -> Const $ f x y
        (Right x, _      ) -> unaryOp (x `f`) w2
        (_,       Right y) -> unaryOp (`f` y) w1

    parseW16 :: String -> Either String Word16
    parseW16 s =
      case reads s of
        [(x, "")] -> Right x
        _         -> Left s

solve :: Input -> Solution
solve = probe const "a"

solve2 :: Input -> Solution
solve2 i = solve $ insert "b" (Const $ solve i) i

render :: Solution -> IO ()
render = print

--- helpers

probe :: (Word16 -> Input -> Word16) -> Wire -> Input -> Word16
probe use w i =
  case i !? w of
    Const v -> use v i
    Cont w' cont' ->
      probe (next cont') w' i
  where
    next cont v i' =
      probe use w (insert w (cont v) i')


(!?) :: Map String v -> String -> v
m !? k = fromMaybe (error $ "Wrong key: " ++ k
                   ++ ", keys: " ++ show (map fst $ toList m))
         $ lookup k m

--- selftesting

worksFine :: Bool
worksFine = exampleWorksFine &&  probeWorksFine

exampleWorksFine :: Bool
exampleWorksFine =
  let i = parse example
  in  all (\(w, v) -> probe const w i == v)
      [("d", 72),
       ("e", 507),
       ("f", 492),
       ("g", 114),
       ("h", 65412),
       ("i", 65079),
       ("x", 123),
       ("y", 456)]


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

probeWorksFine :: Bool
probeWorksFine =
  (15 ==)
  $ probe const "a"
  $ fromList [ ("a", Cont "b" (\b -> Cont "c" (\c -> Const (b + c))))
             , ("b", Const 10)
             , ("c", Cont "d" (\d -> Const (3 + d)))
             , ("d", Const 2) ]
