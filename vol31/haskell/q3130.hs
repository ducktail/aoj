import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> readLn <*> f >>= putStrLn . unwords . map show
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> [Int] -> [Int]
solve n [a, b] = f $ n `mod` 12
  where
    f 0 = [a, b]
    f 1 = [a - b, b]
    f 2 = [a - b, a]
    f 3 = [-b, a]
    f 4 = [-b, a - b]
    f 5 = [-a, a - b]
    f 6 = map negate $ f 0
    f 7 = map negate $ f 1
    f 8 = map negate $ f 2
    f 9 = map negate $ f 3
    f 10 = map negate $ f 4
    f 11 = map negate $ f 5

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
