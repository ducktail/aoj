import Control.Applicative

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [a, b, x] = min (q * a) (2 * q * b) + min a (i * b)
  where
    (q, r) = divMod x 1000
    i = (r + 499) `div` 500
