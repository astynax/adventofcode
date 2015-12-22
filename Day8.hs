module Day8 where

type Code     = String

type Input    = [Code]
type Solution = Int

main :: IO ()
main =
  putStrLn "Part 1:" >> run (solve calculate)
  >>
  putStrLn "Part 2:" >> run (solve calculate2)
  where
    run f = readFile "inputs/day8.txt" >>= render . f . parse

parse :: String -> Input
parse = lines

solve :: (Code -> Int) -> Input -> Solution
solve = (sum .) . map

render :: Solution -> IO ()
render = print

calculate :: Code -> Int
calculate c = codeLength c - byteLength c

calculate2 :: Code -> Int
calculate2 c = encodedLength c - codeLength c

codeLength :: Code -> Int
codeLength = length

-- this solution gets a "wrong" number
-- byteLength = length . (read :: String -> String)

byteLength :: Code -> Int
byteLength = (+ (-2)) . go 0
  where
    go acc [] = acc
    go acc xs =
      go (acc + 1)
      $ case xs of
        ('\\':'\\':ys)     -> ys
        ('\\':'"' :ys)     -> ys
        ('\\':'x' :_:_:ys) -> ys
        (_:ys)             -> ys
        _                  -> []


encodedLength :: Code -> Int
encodedLength s = 2 + go 0 s
  where
    go acc [] = acc
    go acc (x:xs) =
      flip go xs
      $ (acc +) $ case x of
        '"'  -> 2
        '\\' -> 2
        _    -> 1


worksFine :: Bool
worksFine =
  and [ check "\"\""           == (2,  0)
      , check "\"abc\""        == (5,  3)
      , check "\"aaa\\\"aaa\"" == (10, 7)
      , check "\"\\x27\""      == (6,  1)
      ]
  where
    check c = (codeLength c, byteLength c)
