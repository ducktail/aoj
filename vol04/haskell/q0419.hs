import Control.Applicative

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List

import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> B.getLine >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> ByteString -> Int
solve [l, n] bs = 3 * (oo bs) * (2 ^ n - 1) + l

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace

oo :: ByteString -> Int
oo s = f 0 s (B.tail s)
  where
    f c s s'
      | B.null s' = c
      | B.head s == 'o' && B.head s' == 'o' = f (c + 1) (B.tail s) (B.tail s')
      | otherwise = f c (B.tail s) (B.tail s')
