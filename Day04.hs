-- stack --install-ghc runghc --package cryptohash
module Day04 where

import           Crypto.Hash           (Digest, MD5, digestToHexByteString,
                                        hash)
import           Data.ByteString.Char8 (ByteString, append, pack, take)
import           Prelude               hiding (take)

type Guess = (Int, ByteString)

secret :: ByteString
secret = pack "iwrupvqb"

main :: IO ()
main = do
  putStrLn "Part 1:"
  runFor 5

  putStrLn "Part 2:"
  runFor 6

  where
    runFor n =
      print . fst . head
      . filter (isGood n . snd)
      . map (mkGuess secret) $ [1..]

mkGuess :: ByteString -> Int -> Guess
mkGuess sec cnt =
  (,) cnt . hash' . append sec . pack . show $ cnt

isGood :: Int -> ByteString -> Bool
isGood n = (== pack "000000") . take n

hash' :: ByteString -> ByteString
hash' = digestToHexByteString . md5

md5 :: ByteString -> Digest MD5
md5 = hash
