import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  getLine
  [a, d] <- f
  m <- readLn
  solve a d <$> (reverse <$> replicateM m f) <*> readLn >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [[Int]] -> Int -> Int
solve a d xs k = a + (ok-1) * d
  where ok = foldl' f k xs
        f i [x, y, z]
          | x == 0 && y == i = z
          | x == 0 && z == i = y
          | x == 1 && y == i = z
          | otherwise = i
