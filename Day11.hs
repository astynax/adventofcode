module Day11 where

type Password = String

type Input    = Password
type Solution = Password

main :: IO ()
main = do
  let input = "vzbxkghb"
      pass1 = getNext input
      pass2 = getNext pass1

  putStrLn "Part 1:"
  putStrLn pass1

  putStrLn "Part 2:"
  putStrLn pass2


getNext :: Input -> Solution
getNext = head . filter isGood . candidates

--- incrementing

candidates :: Password -> [Password]
candidates = map reverse . tail . iterate succStr . reverse

succStr :: String -> String
succStr "" = "a"
succStr (x:xs)
  | x >= 'a' && x < 'z' = succ x : xs
  | x == 'z'            = 'a' : succStr xs
  | otherwise           = error "Bad argument!"

--- filtering

isGood :: Password -> Bool
isGood x =
  hasThreeIncreasingLetters x
  &&
  hasntIOL x
  &&
  hasTwoAndMoreDoublets x


hasThreeIncreasingLetters, hasntIOL, hasTwoAndMoreDoublets :: Password -> Bool

hasThreeIncreasingLetters xs =
  any (\(a, b, c) -> succ a == b && succ b == c)
  $ zip3 xs (tail xs) (tail $ tail xs)

hasntIOL = not . any (`elem` "iol")

hasTwoAndMoreDoublets =
  not . null . drop 1 . nonOverlapingPairs
  where
    nonOverlapingPairs (a:b:xs)
      | a == b    = [a, b] : nonOverlapingPairs xs
      | otherwise = nonOverlapingPairs (b : xs)
    nonOverlapingPairs _ = []

--- selftesting

worksFine, incrementingWorks, filteringWorks, exampleWorks :: Bool

worksFine =
  incrementingWorks
  &&
  filteringWorks
  &&
  exampleWorks

incrementingWorks =
  let cs = candidates "aaa"
  in head cs == "aab"
     &&
     take 2 (drop (     26 - 2) cs) == ["aaz", "aba"]
     &&
     take 2 (drop (26 * 26 - 2) cs) == ["azz", "baa"]
     &&
     head (tail . dropWhile (/= "zzz") $ cs) == "aaaa"

filteringWorks =
  is "aaa" [not.passesRule3]
  &&
  is "hijklmmn" [passesRule1, not.passesRule2]
  &&
  is "abbceffg" [passesRule3, not.passesRule1]
  &&
  is "abbcegjk" [not.passesRule3]
  where
    passesRule1 = hasThreeIncreasingLetters
    passesRule2 = hasntIOL
    passesRule3 = hasTwoAndMoreDoublets

    is x = all ($ x)

exampleWorks =
  getNext "abcdefgh" == "abcdffaa"
  &&
  getNext "ghijklmn" == "ghjaabcc"
