import Control.Applicative
import Data.List

main :: IO ()
main = do
  [n, t, e] <- f
  solve t e <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> Int
solve t e xs = case find f (zip xs [1..]) of
                Just (_, i) -> i
                Nothing -> (-1)
  where
    f (x, _) = let l = (t - e) `div` x * x
                   u = (t + e) `div` x * x
               in not . null $ intersect [l, l+x .. u] [t-e .. t+e]
