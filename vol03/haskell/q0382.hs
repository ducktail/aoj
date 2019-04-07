import Control.Applicative

main :: IO ()
main = solve <$> f <*> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Int
solve [n, _] ps = (sum ps + n) `div` (n + 1)
