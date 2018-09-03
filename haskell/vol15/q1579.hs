import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- f
  [a, d] <- g
  m <- f
  solve n a d <$> (reverse <$> (replicateM m g)) <*> f >>= print
  where
    f = readi B.readInt <$> B.getLine
    g = readil B.readInt <$> B.getLine

solve :: Int -> Int -> Int -> [[Int]] -> Int -> Int
solve n a d cs k = g $ foldl' f (k, []) cs
  where
    f (i, fs) [x, y, z]
      | x == 1 && y <= i && i <= z = (i, (+1) : fs)
      | x == 2 && y <= i && i <= z = (i, (flip div 2) : fs)
      | x == 0 && y <= i && i <= z = (y+z-i, fs)
      | otherwise = (i, fs)
    g (i, fs) = foldl' (\x f -> f x) (a + (i-1) * d) fs
                    
readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
