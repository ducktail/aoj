import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Text.Printf

main :: IO ()
main = getLine >> solve <$> (sortBy (flip compare) <$> f) >>= printf "%.6f\n"
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Double
solve (w:x:y:z:as) = let re1 = fromIntegral (y + z) / fromIntegral (w - x)
                         re2 = fromIntegral (w + z) / fromIntegral (x - y)
                         re3 = fromIntegral (w + x) / fromIntegral (y - z)
                         re4 = f (fromIntegral (w + x)) 0 (z:as)
                     in maximum [re1, re2, re3, re4]
  where
    f _ mx [_] = mx
    f nr mx (x:y:ys) = f nr (max mx (nr / fromIntegral (x - y))) (y:ys)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')
