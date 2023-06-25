import Control.Applicative
import Data.List

main :: IO ()
main = solve <$> f >>= print
  where
    f = sort <$> map read <$> words <$> getLine

solve :: [Int] -> Int
solve [a, b, c, d]
  | a == b && b == c = 1
  | b == c && c == d = 1
  | a == b && c == d = 1
  | a == b = 3
  | b == c = 3
  | c == d = 3
  | otherwise = 2
