import Control.Applicative
import Data.List
import Data.Bool (bool)

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve as = bool 0 1 $ a0 == a1 && a2 == a3
  where
    [a0, a1, a2, a3] = sort as
