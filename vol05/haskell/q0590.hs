import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n readLn >>= print

solve :: [Int] -> Int
solve = length . filter f
  where
    f x = all (g x) $ takeWhile (\i -> 2 * i * (i + 1) <= x) [1..]
    g x y = (x - y) `mod` (2 * y + 1) /= 0
