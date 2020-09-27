import Control.Applicative

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve xs = mx - (mx + mn) `div` 2
  where
    mx = maximum xs
    mn = minimum xs
