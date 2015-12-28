module Day05 where

type Input    = String
type Solution = Int
type Test     = Input -> Bool

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day5.txt"

  putStrLn "Part 1:"
  runWith isNice input

  putStrLn "part 2:"
  runWith isNice2 input

  where
    runWith f = render . solveWith f

parse :: String -> [Input]
parse = lines

solveWith :: Test -> [Input] -> Solution
solveWith f = length . filter f

render :: Solution -> IO ()
render = print

--- solution for the part #1

isNice :: Test
isNice i = has3Vowels i && hasDoublets i && not (hasBadStrings i)

has3Vowels :: Test
has3Vowels = (>= 3) . length . filter isVowel
  where
    isVowel = (`elem` "aeiou")

hasDoublets :: Test
hasDoublets = hasPairWhich repeats
  where
    repeats = uncurry (==)

hasBadStrings :: Test
hasBadStrings = hasPairWhich isBad
  where
    isBad (x, y) = [x, y] `elem` ["ab", "cd", "pq", "xy"]

hasPairWhich :: ((Char, Char) -> Bool) -> Test
hasPairWhich p i = any p $ zip i (tail i)

--- solution for the part #2

isNice2 :: Test
isNice2 i = hasRepeatingNonOverlappingPairs i && hasRepeatingLetters i

hasRepeatingNonOverlappingPairs :: Test
hasRepeatingNonOverlappingPairs (a:b:c:d:ds) =
  {-
  example run:
  input = "xxyzzxxX"
  1. go ["xy", "xx"]                   "yz" "zxxX"
  2. go ["yz", "xy", "xx"]             "zz" "xxX"
  3. go ["zz", "yz", "xy", "xx"]       "zx" "xX"
  4. go ["zx", "zz", "yz", "xy", "xx"] "xx" "X"
  match! (current pair was appered in the second time!)
  -}
  go [[b, c], [a, b]] [c, d] ds
  where
    go :: [String] -> String -> String -> Bool
    go pairs@(_ : prevs) pair@[_, x] xs
      | pair `elem` prevs = True
      | otherwise =
        case xs of
          (x' : xs') -> go (pair : pairs) [x, x'] xs'
          _          -> False
    go _ _ _ = error "Smth goes wrong!"
hasRepeatingNonOverlappingPairs _ = False

hasRepeatingLetters :: Test
hasRepeatingLetters i = any repeats $ zip i (drop 2 i)
  where
    repeats = uncurry (==)

--- selfcheck

worksFine :: Bool
worksFine =
  isNice "ugknbfddgicrmopn"
  && isNice "aaa"
  && not (isNice "jchzalrnumimnmhp")
  && not (isNice "haegwjzuvuyypxyu")
  && not (isNice "dvszwmarrgswjxmb")

  && isNice2 "qjhvhtzxzqqjkmpb"
  && isNice2 "xxyxx"
  && not (isNice2 "uurcxstgmygtbstg")
  && not (isNice2 "ieodomkazucvgmuy")
