import Control.Applicative

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve = sum
