import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  getLine
  solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve xs = case f 1 [] xs of
            [x] -> x
            [x,y] -> x + y
            ys -> g 0 ys
  where
    f n as [_] = n : as
    f n as (x:y:ys)
      | x /= y = f (n + 1) as (y:ys)
      | otherwise = f 1 (n : as) (y:ys)
    g mx (x:y:z:zs) = g (max mx (x + y + z)) (y:z:zs)
    g mx _ = mx
    
readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')
