import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve [n, a, b, c, d] = min ((n+a-1) `div` a * b) ((n+c-1) `div` c * d)
