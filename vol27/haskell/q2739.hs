import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve [n, m, t] as = head as - m
                     + max 0 (t - last as - m)
                     + sum (tail >>= zipWith f $ as)
  where
    f  x y = max 0 (x - y - 2 * m)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')
