-- stack --install-ghc runghc --package cryptohash
module Day4 where

import Prelude hiding (take)
import Data.ByteString.Char8 (pack, append, ByteString, take)
import Crypto.Hash (digestToHexByteString, hash, Digest, MD5)

type Guess = (Int, ByteString)

secret :: ByteString
secret = pack "iwrupvqb"

main :: IO ()
main = print . fst . head . filter (isGood . snd) . map (mkGuess secret) $ [1..]

mkGuess :: ByteString -> Int -> Guess
mkGuess sec cnt =
  (,) cnt . hash' . append sec . pack . show $ cnt

isGood :: ByteString -> Bool
-- isGood = (== pack "000000") . take 6 -- for the 1st subtask
isGood = (== pack "000000") . take 6

hash' :: ByteString -> ByteString
hash' = digestToHexByteString . md5

md5 :: ByteString -> Digest MD5
md5 = hash
