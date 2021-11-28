import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> readLn <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> [Int] -> Int
solve n vs = sum vs - n * (n + 1) `div` 2

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
