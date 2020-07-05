import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve ps = foldl (\c i -> min c (count i)) 1000000 [0..3]
  where
    count n = let (x:xs) = ps in f n ((max 0 (x - n)):xs)
    f ct [x] = ct + x
    f ct (x:y:ys) = f (ct + 2 * x) ((max 0 (y - x)):ys)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')
